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

# Linux advisory file lock (append only)
import fcntl


# ---------------------------
# Data structures
# ---------------------------

@dataclass
class Rule:
    number: str
    severity: str  # E/W/I
    description: str
    code: str      # regex (used when rtype == REGEX)
    run: bool      # Y/N

    # Rule type and parameters
    rtype: str = "REGEX"          # REGEX | MAX_SECTIONS | REQUIRED_SECTIONS | REQUIRED_DIVISIONS
    max_value: int = 0            # used for MAX_SECTIONS
    required_list: List[str] = None  # used for REQUIRED_* rules

    # Scope control
    on_master: bool = True
    on_local: bool = True
    on_diff: bool = True


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
    Lock scope is ONLY the append operation.
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
    """
    Strip:
      - empty lines
      - whitespace-only lines
      - comment lines starting with '*'
    Keep:
      - 'pure COBOL code' lines (trim trailing spaces)
    """
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
    """
    Returns list of (original_line_number, cleaned_line_text)
    Cleaning rules:
      - strip inline comments (*> ...)
      - skip full-line comments starting with '*'
      - skip blank/space-only lines
      - rstrip trailing spaces
    """
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


def parse_yn(attr_val: str, default: bool) -> bool:
    if attr_val is None:
        return default
    v = attr_val.strip().upper()
    if v == "Y":
        return True
    if v == "N":
        return False
    return default


def parse_required_list(s: str) -> List[str]:
    if not s:
        return []
    return [x.strip().upper() for x in s.split(",") if x.strip()]


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

        rtype = (r.get("type") or "REGEX").strip().upper()

        # Defaults for scopes (backward-friendly):
        # - REGEX defaults to master+local+diff
        # - Non-regex defaults to local-only unless specified
        if rtype == "REGEX":
            def_master, def_local, def_diff = True, True, True
        else:
            def_master, def_local, def_diff = False, True, False

        on_master = parse_yn(r.get("on_master"), def_master)
        on_local = parse_yn(r.get("on_local"), def_local)
        on_diff = parse_yn(r.get("on_diff"), def_diff)

        max_value = 0
        if rtype == "MAX_SECTIONS":
            try:
                max_value = int((r.get("max") or "0").strip())
            except ValueError:
                max_value = 0

        required_list = parse_required_list(r.get("required") or "")

        desc_el = r.find("./Description")
        code_el = r.find("./Code")

        description = (desc_el.text or "").strip() if desc_el is not None else ""
        code = (code_el.text or "").strip() if code_el is not None else ""

        # Validation
        if not number or severity not in ("E", "W", "I"):
            continue

        if rtype == "REGEX" and not code:
            continue

        if rtype == "MAX_SECTIONS" and max_value <= 0:
            continue

        if rtype in ("REQUIRED_SECTIONS", "REQUIRED_DIVISIONS") and not required_list:
            continue

        rules.append(
            Rule(
                number=number,
                severity=severity,
                description=description,
                code=code,
                run=run,
                rtype=rtype,
                max_value=max_value,
                required_list=required_list,
                on_master=on_master,
                on_local=on_local,
                on_diff=on_diff,
            )
        )

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
    """
    Return lines present in private but not in master (multiset-aware).
    """
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


def get_section_decls_from_source(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[str, int]]:
    """
    Return section declarations in source order as (SECTION-NAME, line_number),
    excluding WORKING-STORAGE and LINKAGE.
    """
    sec_rx = re.compile(r"^\s*([A-Z0-9][A-Z0-9-]*)\s+SECTION\s*\.", re.IGNORECASE)
    out: List[Tuple[str, int]] = []
    for lnno, txt in cob_lines_with_linenos:
        m = sec_rx.search(txt)
        if not m:
            continue
        name = (m.group(1) or "").upper()
        if name in ("WORKING-STORAGE", "LINKAGE"):
            continue
        out.append((name, lnno))
    return out


def find_divisions_in_source(cob_lines_with_linenos: List[Tuple[int, str]]) -> Dict[str, int]:
    """
    Return dict: division_name -> first line number.
    Division names are upper-case words like IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE.
    """
    div_rx = re.compile(r"^\s*([A-Z0-9-]+)\s+DIVISION\s*", re.IGNORECASE)
    found: Dict[str, int] = {}
    for lnno, txt in cob_lines_with_linenos:
        m = div_rx.search(txt)
        if not m:
            continue
        name = (m.group(1) or "").upper()
        if name not in found:
            found[name] = lnno
    return found


def find_procedure_division_line(cob_lines_with_linenos: List[Tuple[int, str]]) -> int:
    divs = find_divisions_in_source(cob_lines_with_linenos)
    return divs.get("PROCEDURE", 1)


def rule_runs_in_context(rule: Rule, context: str) -> bool:
    if context == "MASTER":
        return rule.on_master
    if context == "LOCAL":
        return rule.on_local
    if context == "DIFF":
        return rule.on_diff
    return False


def apply_rules(
    lines: List[str],
    rules: List[Rule],
    context: str,
    cob_lines_with_linenos: List[Tuple[int, str]] = None,
    max_samples_per_rule: int = 5
) -> Tuple[Dict[str, int], List[RuleMatch]]:
    """
    Apply rules to the given cleaned lines in a given context (MASTER/LOCAL/DIFF).
    For REQUIRED_* rules we use the COBOL source index for accuracy and line numbers.
    """
    counts = {"E": 0, "W": 0, "I": 0}
    matches: List[RuleMatch] = []

    for rule in rules:
        if not rule.run:
            continue
        if not rule_runs_in_context(rule, context):
            continue

        # --------------------
        # Special: MAX_SECTIONS
        # --------------------
        if rule.rtype == "MAX_SECTIONS":
            sec_rx = re.compile(r"^\s*([A-Z0-9][A-Z0-9-]*)\s+SECTION\s*\.", re.IGNORECASE)
            seen = []
            seen_set = set()

            for ln in lines:
                m = sec_rx.search(ln)
                if not m:
                    continue
                nm = (m.group(1) or "").upper()
                if nm in ("WORKING-STORAGE", "LINKAGE"):
                    continue
                if nm not in seen_set:
                    seen_set.add(nm)
                    seen.append(nm)

            section_count = len(seen)

            if section_count > rule.max_value:
                counts[rule.severity] += 1

                samples: List[str] = [f"SECTION count={section_count} (max={rule.max_value})"]
                for n in seen[: min(len(seen), max_samples_per_rule)]:
                    samples.append(f"SECTION: {n}")

                # Best effort line number: (max+1)th unique SECTION declaration in source order
                src_locs: List[int] = []
                if cob_lines_with_linenos:
                    decls = get_section_decls_from_source(cob_lines_with_linenos)
                    uniq_order: List[Tuple[str, int]] = []
                    uniq_set = set()
                    for nm, lnno in decls:
                        if nm not in uniq_set:
                            uniq_set.add(nm)
                            uniq_order.append((nm, lnno))
                    idx = rule.max_value
                    if idx < len(uniq_order):
                        src_locs = [uniq_order[idx][1]]

                matches.append(RuleMatch(rule=rule, count=1, sample_lines=samples, src_locations=src_locs))
            continue

        # --------------------
        # Special: REQUIRED_SECTIONS
        # --------------------
        if rule.rtype == "REQUIRED_SECTIONS":
            if not cob_lines_with_linenos:
                # no source index => cannot evaluate accurately
                continue

            decls = get_section_decls_from_source(cob_lines_with_linenos)
            present = {nm for nm, _ in decls}
            required = set((rule.required_list or []))

            missing = sorted([x for x in required if x not in present])
            if missing:
                counts[rule.severity] += 1

                proc_ln = find_procedure_division_line(cob_lines_with_linenos)
                samples = [f"Missing SECTION(s): {', '.join(missing)}"]
                src_locs = [proc_ln]  # anchor diagnostic at PROCEDURE DIVISION

                matches.append(RuleMatch(rule=rule, count=1, sample_lines=samples, src_locations=src_locs))
            continue

        # --------------------
        # Special: REQUIRED_DIVISIONS
        # --------------------
        if rule.rtype == "REQUIRED_DIVISIONS":
            if not cob_lines_with_linenos:
                continue

            divs = find_divisions_in_source(cob_lines_with_linenos)
            present = set(divs.keys())
            required = set((rule.required_list or []))

            missing = sorted([x for x in required if x not in present])
            if missing:
                counts[rule.severity] += 1

                # anchor at first line (or IDENTIFICATION if exists)
                anchor = divs.get("IDENTIFICATION", 1)
                samples = [f"Missing DIVISION(s): {', '.join(missing)}"]
                src_locs = [anchor]

                matches.append(RuleMatch(rule=rule, count=1, sample_lines=samples, src_locations=src_locs))
            continue

        # --------------------
        # Default: REGEX
        # --------------------
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
            counts[rule.severity] += hit

            src_locs: List[int] = []
            if cob_lines_with_linenos is not None:
                for lnno, txt in cob_lines_with_linenos:
                    if rx.search(txt):
                        src_locs.append(lnno)

            matches.append(
                RuleMatch(
                    rule=rule,
                    count=hit,
                    sample_lines=samples,
                    src_locations=sorted(set(src_locs)),
                )
            )

    return counts, matches


def format_rule_matches(title: str, matches: List[RuleMatch], cob_filename: str) -> str:
    if not matches:
        return f"{title}\nNo rule matches.\n"

    sev_order = {"E": 0, "W": 1, "I": 2}
    sev_word = {"E": "Error", "W": "Warning", "I": "Info"}

    matches_sorted = sorted(matches, key=lambda m: (sev_order.get(m.rule.severity, 9), m.rule.number))

    out: List[str] = []
    out.append(f"{title}\n")
    out.append("--------------------------------------------------\n")
    for m in matches_sorted:
        out.append(f"[{m.rule.severity}] Rule {m.rule.number}: {m.rule.description}\n")
        if m.rule.rtype == "REGEX":
            out.append(f"   Regex : {m.rule.code}\n")
        elif m.rule.rtype == "MAX_SECTIONS":
            out.append(f"   Type  : MAX_SECTIONS (max={m.rule.max_value})\n")
        else:
            out.append(f"   Type  : {m.rule.rtype} (required={','.join(m.rule.required_list or [])})\n")

        out.append(f"   Hits  : {m.count}\n")

        if m.sample_lines:
            out.append("   Samples:\n")
            for s in m.sample_lines:
                out.append(f"     - {s}\n")

        sw = sev_word.get(m.rule.severity, "Info")
        if m.src_locations:
            for lnno in m.src_locations:
                out.append(f"   {cob_filename}:{lnno}: {sw}: ACRT Rule {m.rule.number}: {m.rule.description}\n")
        else:
            out.append(f"   {cob_filename}:?: {sw}: ACRT Rule {m.rule.number}: {m.rule.description} (line not found)\n")

        out.append("\n")

    return "".join(out)


def diagnostics_only(matches: List[RuleMatch], cob_filename: str, severities: Tuple[str, ...] = ("E",)) -> str:
    """
    Spool output (stdout): show ONLY diagnostics lines.
    """
    sev_word = {"E": "Error", "W": "Warning", "I": "Info"}

    out: List[str] = []
    for m in matches:
        if m.rule.severity not in severities:
            continue
        sw = sev_word.get(m.rule.severity, "Info")
        if m.src_locations:
            for lnno in m.src_locations:
                out.append(f"   {cob_filename}:{lnno}: {sw}: ACRT Rule {m.rule.number}: {m.rule.description}")
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

    # Read + clean listing files
    master_clean = clean_listing_text(read_text(master_lis))
    private_clean = clean_listing_text(read_text(private_lis))

    # Build COBOL source index for line number reporting
    cob_lines_with_linenos = clean_cobol_source_with_linenos(src_path)

    # Diff lines
    diff_lines = compute_local_only_diff(master_clean, private_clean)

    # Apply rules per context
    master_counts, master_matches = apply_rules(
        master_clean, rules, context="MASTER", cob_lines_with_linenos=cob_lines_with_linenos
    )
    local_counts, local_matches = apply_rules(
        private_clean, rules, context="LOCAL", cob_lines_with_linenos=cob_lines_with_linenos
    )
    diff_counts, diff_matches = apply_rules(
        diff_lines, rules, context="DIFF", cob_lines_with_linenos=cob_lines_with_linenos
    )

    # Differences (Private - Master) from MASTER/LOCAL-scoped evaluations
    diff_epilog = {
        "E": local_counts["E"] - master_counts["E"],
        "W": local_counts["W"] - master_counts["W"],
        "I": local_counts["I"] - master_counts["I"],
    }

    # Add DIFF-only rules into the epilog delta (rules that run only on DIFF)
    # (i.e., on_diff=Y and on_master=on_local=N)
    diff_only_counts = {"E": 0, "W": 0, "I": 0}
    for m in diff_matches:
        r = m.rule
        if r.on_diff and (not r.on_master) and (not r.on_local):
            diff_only_counts[r.severity] += m.count

    diff_epilog["E"] += diff_only_counts["E"]
    diff_epilog["W"] += diff_only_counts["W"]
    diff_epilog["I"] += diff_only_counts["I"]

    # Threshold decision based on absolute differences
    abs_err = abs(diff_epilog["E"])
    abs_wrn = abs(diff_epilog["W"])
    abs_inf = abs(diff_epilog["I"])

    fail = (
        abs_err > thresholds.get("errors", 0)
        or abs_wrn > thresholds.get("warnings", 0)
        or abs_inf > thresholds.get("infos", 0)
    )

    # Full report (goes to STATUS FILE)
    report_lines: List[str] = []
    report_lines.append(f"Getting current ACRT status for {base} on {now_str()} (user: {current_user()}):\n")
    report_lines.append(f"On Master build directory : #Errors {master_counts['E']} #Warnings {master_counts['W']} #Infos {master_counts['I']}\n")
    report_lines.append(f"On Private build directory : #Errors {local_counts['E']} #Warnings {local_counts['W']} #Infos {local_counts['I']}\n")
    report_lines.append(
        "The difference : "
        f"Errors {diff_epilog['E']} "
        f"Warnings {diff_epilog['W']} "
        f"Infos {diff_epilog['I']}\n"
    )
    report_lines.append("\n")

    # Keep your original key view: local-only diff matches
    report_lines.append(format_rule_matches(
        "Rule matches on LOCAL-ONLY DIFF (Private - Master):",
        diff_matches,
        base
    ))
    report_lines.append("\n")

    # Also include local-only and master-only context results (useful for REQUIRED_* rules)
    report_lines.append(format_rule_matches(
        "Rule matches on LOCAL LISTING (Private build):",
        local_matches,
        base
    ))
    report_lines.append("\n")

    report_lines.append(format_rule_matches(
        "Rule matches on MASTER LISTING (AML build):",
        master_matches,
        base
    ))
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

    # Spool output (stdout): ONLY error diagnostics lines (from ALL contexts)
    all_matches_for_stdout = diff_matches + local_matches + master_matches
    diag = diagnostics_only(all_matches_for_stdout, base, severities=("E",))
    if diag:
        print(diag, end="")

    # Status file: full report append with lock (prevents concurrent corruption)
    append_text_locked(status_path, report + "\n" + ("=" * 80) + "\n")

    sys.exit(1 if fail else 0)


def parse_command_line():
    parser = argparse.ArgumentParser(
        description="nltc-acrt - COBOL audit on listing files (Master vs Private) and local-only diff"
    )
    parser.add_argument("cobfile", help="COBOL source file name (e.g., name.cob / name.pco / name.inc)")
    return parser.parse_args()


if __name__ == "__main__":
    main()
