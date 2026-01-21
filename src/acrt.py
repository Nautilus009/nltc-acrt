import argparse
import os
import sys
from typing import List, Tuple

from core import (
    ACCEPTED_EXTS,
    acrt_tag,
    append_text_locked,
    clean_cobol_source_with_linenos,
    clean_listing_text,
    current_user,
    error_exit,
    map_listing_lines_to_cob_lines,
    now_str,
    read_text,
    strip_known_ext,
    write_text,
    find_listing_paths,
)
from engine import apply_rules, compute_local_only_diff, diagnostics_only, format_rule_matches
from rules import load_rules_config


def main() -> None:
    pa = parse_command_line()

    if getattr(pa, "version", False):
        print(acrt_tag())
        sys.exit(0)

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

    expected_prog_base = strip_known_ext(base).upper()

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

    master_lines_with_linenos = list(enumerate(master_clean, start=1))
    local_lines_with_linenos = list(enumerate(private_clean, start=1))

    master_line_map = map_listing_lines_to_cob_lines(master_clean, cob_lines_with_linenos)
    local_line_map = map_listing_lines_to_cob_lines(private_clean, cob_lines_with_linenos)

    # Diff lines
    diff_lines = compute_local_only_diff(master_clean, private_clean)
    diff_lines_with_linenos = list(enumerate(diff_lines, start=1))
    diff_line_map = map_listing_lines_to_cob_lines(diff_lines, cob_lines_with_linenos)

    # Apply rules per context
    master_counts, master_matches = apply_rules(
        master_clean,
        rules,
        context="MASTER",
        expected_prog_base=expected_prog_base,
        cob_lines_with_linenos=master_lines_with_linenos,
        line_map=master_line_map,
    )
    local_counts, local_matches = apply_rules(
        private_clean,
        rules,
        context="LOCAL",
        expected_prog_base=expected_prog_base,
        cob_lines_with_linenos=local_lines_with_linenos,
        line_map=local_line_map,
    )
    diff_counts, diff_matches = apply_rules(
        diff_lines,
        rules,
        context="DIFF",
        expected_prog_base=expected_prog_base,
        cob_lines_with_linenos=diff_lines_with_linenos,
        line_map=diff_line_map,
    )

    # Differences (Private - Master) from MASTER/LOCAL-scoped evaluations
    diff_epilog = {
        "E": local_counts["E"] - master_counts["E"],
        "W": local_counts["W"] - master_counts["W"],
        "I": local_counts["I"] - master_counts["I"],
    }

    # Add DIFF-only rules into the epilog delta (rules that run only on DIFF)
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

    fail = False
    if abs_err > thresholds.get("errors", 0):
        fail = True
    if abs_wrn > thresholds.get("warnings", 0):
        fail = True
    if abs_inf > thresholds.get("infos", 0):
        fail = True

    report_lines: List[str] = []
    report_lines.append(
        f"Getting current {acrt_tag()} status for {base} on {now_str()} (user: {current_user()}):\n"
    )
    report_lines.append(
        f"On Master build directory : #Errors {master_counts['E']} #Warnings {master_counts['W']} #Infos {master_counts['I']}\n"
    )
    report_lines.append(
        f"On Private build directory : #Errors {local_counts['E']} #Warnings {local_counts['W']} #Infos {local_counts['I']}\n"
    )
    report_lines.append(
        f"The difference : Errors {diff_epilog['E']} Warnings {diff_epilog['W']} Infos {diff_epilog['I']}\n"
    )
    report_lines.append("\n")
    report_lines.append("ACRT RULES REPORT\n")
    report_lines.append("==================================================\n")

    report_lines.append(format_rule_matches(
        "Rule matches on LOCAL LISTING (Private build):",
        local_matches,
        base
    ))
    report_lines.append("\n")

    report_lines.append(format_rule_matches(
        "Rule matches on MASTER LISTING (ALM build):",
        master_matches,
        base
    ))
    report_lines.append("\n")

    report_lines.append(format_rule_matches(
        "Rule matches on LOCAL-ONLY DIFF (Private - Master):",
        diff_matches,
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
        report_lines.append(f"RESULT: ERROR - the following {base} program does not meet {acrt_tag()} standards.\n")
    else:
        report_lines.append(f"RESULT: SUCCESS - the following {base} program meets {acrt_tag()} standards.\n")

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
    parser.add_argument("-version", "--version", action="store_true", help="Print tool version and exit")
    return parser.parse_args()


if __name__ == "__main__":
    main()
