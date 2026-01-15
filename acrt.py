#!/usr/bin/env python3
import argparse
import datetime
import getpass
import os
import re
import sys
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from typing import Dict, List, Tuple

# Linux advisory file lock
import fcntl


# ---------------------------
# Data structures
# ---------------------------

@dataclass
class Rule:
    number: str
    severity: str  # E/W/I
    description: str
    code: str      # regex
    run: bool      # Y/N


@dataclass
class RuleMatch:
    rule: Rule
    count: int
    sample_lines: List[str]
    src_locations: List[int]  # COBOL source line numbers where the rule matches


# ---------------------------
# Utilities
# ---------------------------

ACCEPTED_EXTS = (".cob", ".inc", ".pco")


def error_exit(message: str, rc: int = 2) -> None:
    print(f"Error: {message}", file=sys.stderr)
    sys.exit(rc)


def now_str() -> str:
    return datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")


def current_user() -> str:
    try:
        return getpass.getuser()
    except Exception:
        return os.environ.get("USER", "unknown")


def read_text(path: str, encoding: str = "iso-8859-8") -> str:
    try:
        with open(path, "r", encoding=encoding, errors="replace") as f:
            return f.read()
    except Exception as e:
        error_exit(f"Failed reading file: {path}: {e}")


def write_text(path: str, text: str, encoding: str = "iso-8859-8") -> None:
    try:
        with open(path, "w", encoding=encoding, errors="replace") as f:
            f.write(text)
    except Exception as e:
        error_exit(f"Failed writing file {path}: {e}")


def append_text_locked(path: str, text: str, encoding: str = "iso-8859-8") -> None:
    """
    Append to a report file with an exclusive advisory lock.
    Lock scope is ONLY the append operation (as requested).
    """
    try:
        with open(path, "a", encoding=encoding, errors="replace") as f:
            fcntl.flock(f.fileno(), fcntl.LOCK_EX)
            try:
                f.write(text)
                f.flush()
                os.fsync(f.fileno())
            finally:
                fcntl.flock(f.fileno(), fcntl.LOCK_UN)
    except Exception as e:
        error_exit(f"Failed appending file {path} (locked): {e}")


def clean_listing_text(text: str) -> List[str]:
    out: List[str] = []
    for line in text.splitlines():
        if not line:
            continue
        if line.startswith("*"):
            continue
        if line.strip() == "":
            continue
        out.append(line.rstrip())
    return out


def clean_cobol_source_with_linenos(path: str, encoding: str = "iso-8859-8") -> List[Tuple[int, str]]:
    text = read_text(path, encoding=encoding)
    out: List[Tuple[int, str]] = []
    for i, line in enumerate(text.splitlines(), start=1):
        line = re.sub(r"\*>.*$", "", line)   # strip inline comment
        if line.startswith("*"):
            continue
        if line.strip() == "":
            continue
        out.append((i, line.rstrip()))
    return out


def find_source_locations_for_rule(rule_rx: re.Pattern, cob_lines: List[Tuple[int, str]]) -> List[int]:
    locs: List[int] = []
    for ln, txt in cob_lines:
        if rule_rx.search(txt):
            locs.append(ln)
    return locs


def load_rules_config(xml_path: str) -> Tuple[List[Rule], Dict[str, int]]:
    if not os.path.isfile(xml_path):
        error_exit(f"Rules config not found: {xml_path}")

    try:
        tree = ET.parse(xml_path)
        root = tree.getroot()
    except Exception as e:
        error_exit(f"Failed parsing rules XML: {xml_path}: {e}")

    thresholds = {"errors": 0, "warnings": 0, "infos": 0}

    th = root.find(".//Thresholds")
    if th is not None:
        thresholds["errors"] = int(th.get("errors", thresholds["errors"]))
        thresholds["warnings"] = int(th.get("warnings", thresholds["warnings"]))
        thresholds["infos"] = int(th.get("infos", thresholds["infos"]))

    rules: List[Rule] = []
    for r in root.findall(".//Rule"):
        number = (r.get("number") or "").strip()
        severity = (r.get("severity") or "").strip().upper()
        run = ((r.get("run") or "N").strip().upper() == "Y")

        desc_el = r.find("./Description")
        code_el = r.find("./Code")

        description = (desc_el.text or "").strip() if desc_el is not None else ""
        code = (code_el.text or "").strip() if code_el is not None else ""

        if not number or severity not in ("E", "W", "I") or not code:
            continue

        rules.append(Rule(number=number, severity=severity, description=description, code=code, run=run))

    if not rules:
        print(f"Warning: No valid rules loaded from {xml_path}", file=sys.stderr)

    return rules, thresholds


def strip_known_ext(filename: str) -> str:
    lower = filename.lower()
    for ext in ACCEPTED_EXTS:
        if lower.endswith(ext):
            return filename[: -len(ext)]
    return os.path.splitext(filename)[0]


def find_listing_paths(source_path: str) -> Tuple[str, str]:
    base = os.path.basename(source_path)
    lower = base.lower()

    if not any(lower.endswith(ext) for ext in ACCEPTED_EXTS):
        error_exit(f"Input must be one of: {', '.join(ACCEPTED_EXTS)}  (got: {base})")

    name_no_ext = strip_known_ext(base)
    lis_name = f"{name_no_ext}.lis"

    alm = os.environ.get("BUILD_ALM_PATH_BB", "")
    loc = os.environ.get("BUILD_LOCAL_PATH_BB", "")

    if not alm:
        error_exit("Environment variable BUILD_ALM_PATH_BB is not set.")
    if not loc:
        error_exit("Environment variable BUILD_LOCAL_PATH_BB is not set.")

    alm_lis = os.path.join(alm, "target", "obj", lis_name)
    loc_lis = os.path.join(loc, "target", "obj", lis_name)

    if not os.path.isfile(alm_lis):
        error_exit(f"Master listing file not found: {alm_lis}")
    if not os.path.isfile(loc_lis):
        error_exit(f"Private listing file not found: {loc_lis}")

    return alm_lis, loc_lis


def compute_local_only_diff(master_lines: List[str], private_lines: List[str]) -> List[str]:
    counts: Dict[str, int] = {}
    for ln in master_lines:
        counts[ln] = counts.get(ln, 0) + 1

    diff: List[str] = []
    for ln in private_lines:
        c = counts.get(ln, 0)
        if c > 0:
            counts[ln] = c - 1
        else:
            diff.append(ln)

    return diff


def apply_rules(
    lines: List[str],
    rules: List[Rule],
    cob_lines_with_linenos: List[Tuple[int, str]] = None,
    max_samples_per_rule: int = 5
) -> Tuple[Dict[str, int], List[RuleMatch]]:
    counts = {"E": 0, "W": 0, "I": 0}
    matches: List[RuleMatch] = []

    for rule in rules:
        if not rule.run:
            continue
        try:
            rx = re.compile(rule.code, re.IGNORECASE)
        except re.error as e:
            print(f"Warning: Invalid regex in rule {rule.number}: {e}", file=sys.stderr)
            continue

        hit = 0
        samples: List[str] = []
        for ln in lines:
            if rx.search(ln):
                hit += 1
                if len(samples) < max_samples_per_rule:
                    samples.append(ln)

        if hit > 0:
            if rule.severity in counts:
                counts[rule.severity] += hit

            src_locs: List[int] = []
            if cob_lines_with_linenos is not None:
                src_locs = find_source_locations_for_rule(rx, cob_lines_with_linenos)

            matches.append(RuleMatch(rule=rule, count=hit, sample_lines=samples, src_locations=src_locs))

    return counts, matches


def format_rule_matches(matches: List[RuleMatch], cob_filename: str) -> str:
    if not matches:
        return "No rule matches on local-only diff.\n"

    sev_order = {"E": 0, "W": 1, "I": 2}
    sev_word = {"E": "Error", "W": "Warning", "I": "Info"}

    matches_sorted = sorted(matches, key=lambda m: (sev_order.get(m.rule.severity, 9), m.rule.number))

    out: List[str] = []
    out.append("Rule matches on LOCAL-ONLY DIFF (Private - Master):\n")
    out.append("--------------------------------------------------\n")
    for m in matches_sorted:
        out.append(f"[{m.rule.severity}] Rule {m.rule.number}: {m.rule.description}\n")
        out.append(f"   Regex : {m.rule.code}\n")
        out.append(f"   Hits  : {m.count}\n")
        if m.sample_lines:
            out.append("   Samples:\n")
            for s in m.sample_lines:
                out.append(f"     - {s}\n")

        sw = sev_word.get(m.rule.severity, "Info")
        if m.src_locations:
            for ln in m.src_locations:
                out.append(f"   {cob_filename}:{ln}: {sw}: ACRT Rule {m.rule.number}: {m.rule.description}\n")
        else:
            out.append(f"   {cob_filename}:?: {sw}: ACRT Rule {m.rule.number}: {m.rule.description} (line not found)\n")

        out.append("\n")

    return "".join(out)


def diagnostics_only(matches: List[RuleMatch], cob_filename: str, severities: Tuple[str, ...] = ("E",)) -> str:
    sev_word = {"E": "Error", "W": "Warning", "I": "Info"}

    out: List[str] = []
    for m in matches:
        if m.rule.severity not in severities:
            continue
        sw = sev_word.get(m.rule.severity, "Info")
        if m.src_locations:
            for ln in m.src_locations:
                out.append(f"   {cob_filename}:{ln}: {sw}: ACRT Rule {m.rule.number}: {m.rule.description}")
        else:
            out.append(f"   {cob_filename}:?: {sw}: ACRT Rule {m.rule.number}: {m.rule.description} (line not found)")

    out = sorted(out)
    return "\n".join(out) + ("\n" if out else "")


# ---------------------------
# Main
# ---------------------------

def main() -> None:
    pa = parse_command_line()

    acrt_home = os.environ.get("ACRT_HOME", "")
    if not acrt_home:
        error_exit("Environment variable ACRT_HOME is not set.")
    if not os.path.isdir(acrt_home):
        error_exit(f"ACRT_HOME does not point to an existing directory: {acrt_home}")

    conf_path = os.path.join(acrt_home, "CONF", "ACRT_RULES.XML")
    rules, thresholds = load_rules_config(conf_path)

    src_path = os.path.abspath(pa.cobfile)
    if not os.path.isfile(src_path):
        error_exit(f"COBOL source file not found: {src_path}")

    base = os.path.basename(src_path)
    lower = base.lower()
    if not any(lower.endswith(ext) for ext in ACCEPTED_EXTS):
        error_exit(f"Input must be one of: {', '.join(ACCEPTED_EXTS)}  (got: {base})")

    # Status file: <sourcefile>_acrt under $ACRT_HOME
    status_name = base + "_acrt"
    status_path = os.path.join(acrt_home, status_name)
    if not os.path.exists(status_path):
        write_text(status_path, "")  # create

    master_lis, private_lis = find_listing_paths(src_path)

    master_clean = clean_listing_text(read_text(master_lis))
    private_clean = clean_listing_text(read_text(private_lis))

    cob_lines_with_linenos = clean_cobol_source_with_linenos(src_path)

    master_counts, _ = apply_rules(master_clean, rules)
    private_counts, _ = apply_rules(private_clean, rules)

    diff_lines = compute_local_only_diff(master_clean, private_clean)
    _, diff_matches = apply_rules(diff_lines, rules, cob_lines_with_linenos=cob_lines_with_linenos)

    diff_epilog = {
        "E": private_counts["E"] - master_counts["E"],
        "W": private_counts["W"] - master_counts["W"],
        "I": private_counts["I"] - master_counts["I"],
    }

    abs_err = abs(diff_epilog["E"])
    abs_wrn = abs(diff_epilog["W"])
    abs_inf = abs(diff_epilog["I"])

    fail = (
        abs_err > thresholds.get("errors", 0)
        or abs_wrn > thresholds.get("warnings", 0)
        or abs_inf > thresholds.get("infos", 0)
    )

    report_lines: List[str] = []
    report_lines.append(f"Getting current ACRT status for {base} on {now_str()} (user: {current_user()}):\n")
    report_lines.append(f"On Master build directory : #Errors {master_counts['E']} #Warnings {master_counts['W']} #Infos {master_counts['I']}\n")
    report_lines.append(f"On Private build directory : #Errors {private_counts['E']} #Warnings {private_counts['W']} #Infos {private_counts['I']}\n")
    report_lines.append(
        "The difference : "
        f"Errors {diff_epilog['E']} "
        f"Warnings {diff_epilog['W']} "
        f"Infos {diff_epilog['I']}\n"
    )
    report_lines.append("\n")
    report_lines.append(format_rule_matches(diff_matches, base))
    report_lines.append("\n")
    report_lines.append(
        f"Thresholds: Errors>{thresholds.get('errors', 0)} "
        f"Warnings>{thresholds.get('warnings', 0)} "
        f"Infos>{thresholds.get('infos', 0)}\n"
    )
    report_lines.append(f"Current abs diff: Errors={abs_err} Warnings={abs_wrn} Infos={abs_inf}\n")
    report_lines.append("\n")

    if fail:
        report_lines.append(f"RESULT: ERROR - the following {base} program does not meet ACRT standards.\n")
    else:
        report_lines.append(f"RESULT: SUCCESS - the following {base} program meets ACRT standards.\n")

    report_lines.append(f"Report generated at: {now_str()}\n")
    report = "".join(report_lines)

    # Spool output (stdout): ONLY error diagnostics lines
    diag = diagnostics_only(diff_matches, base, severities=("E",))
    if diag:
        print(diag, end="")

    # Status file: full report append with lock (prevents concurrent corruption)
    append_text_locked(status_path, report + "\n" + ("=" * 80) + "\n")

    sys.exit(1 if fail else 0)


def parse_command_line():
    parser = argparse.ArgumentParser(
        description="ACRT POC - COBOL audit on listing files (Master vs Private) and local-only diff"
    )
    parser.add_argument("cobfile", help="COBOL source file name (e.g., name.cob / name.pco / name.inc)")
    return parser.parse_args()


if __name__ == "__main__":
    main()
