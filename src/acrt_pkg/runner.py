import os
from dataclasses import dataclass
from . import __version__
from .config import load_rules
from .listing import read_listing, clean_listing, clean_source
from .diff import local_only_diff
from .tokenize import tokenize, format_tree, format_tree_verbose
from .rules import RuleEngine, SEVERITY_LABELS
from .report import build_report
from .util import atomic_write, now_timestamp, extract_program_id


@dataclass
class RunResult:
    exit_code: int
    actionable: list
    report_text: str


def _counts_by_severity(findings):
    counts = {"E": 0, "W": 0, "I": 0}
    for f in findings:
        counts[f.severity] += 1
    return counts


def _rule_number_key(rule_number):
    parts = []
    for part in str(rule_number).split("."):
        if part.isdigit():
            parts.append(int(part))
        else:
            parts.append(part)
    return tuple(parts)


def _sort_findings(findings):
    return sorted(
        findings,
        key=lambda f: (_rule_number_key(f.rule_number), f.line is None, f.line or 0),
    )


def run_acrt(element_path, env, debug=False, debug_tree=False):
    element_name = os.path.basename(element_path)
    file_stem, _ = os.path.splitext(element_name)

    rules_path = os.path.join(env["ACRT_HOME"], "CONF", "ACRT_RULES.XML")
    thresholds, rules = load_rules(rules_path)

    master_path = os.path.join(env["BUILD_ALM_PATH_BB"], "target", "obj", f"{file_stem}.lis")
    local_path = os.path.join(env["BUILD_LOCAL_PATH_BB"], "target", "obj", f"{file_stem}.lis")

    if not os.path.exists(master_path) or not os.path.exists(local_path):
        raise FileNotFoundError("Missing master or local listing file")

    master_raw = read_listing(master_path)
    local_raw = read_listing(local_path)
    source_raw = read_listing(element_path)
    source_cleaned = clean_source(source_raw)

    master_clean = clean_listing(master_raw, master_path, source_cleaned=source_cleaned)
    local_clean = clean_listing(local_raw, local_path, source_cleaned=source_cleaned)
    diff_clean = local_only_diff(master_clean, local_clean)

    source_program_id = None
    source_program_line = None
    for text, line_no, _c1 in source_cleaned:
        program_id = extract_program_id(text)
        if program_id:
            source_program_id = program_id
            source_program_line = line_no
            break

    master_ctx = tokenize(master_clean, file_stem)
    local_ctx = tokenize(local_clean, file_stem)
    diff_ctx = tokenize(diff_clean, file_stem)
    for ctx in (master_ctx, local_ctx, diff_ctx):
        ctx.meta["source_program_id"] = source_program_id
        ctx.meta["source_program_line"] = source_program_line
        ctx.meta["source_path"] = element_path
        ctx.meta["tokenized_once"] = True

    if debug:
        print("=== TOKENIZATION TREE: MASTER ===")
        print(format_tree(master_ctx))
        print("=== TOKENIZATION TREE: LOCAL ===")
        print(format_tree(local_ctx))
        print("=== TOKENIZATION TREE: DIFF ===")
        print(format_tree(diff_ctx))
    if debug_tree:
        debug_lines = [
            "=== TOKENIZATION TREE DETAILS: MASTER ===",
            format_tree_verbose(master_ctx),
            "=== TOKENIZATION TREE DETAILS: LOCAL ===",
            format_tree_verbose(local_ctx),
            "=== TOKENIZATION TREE DETAILS: DIFF ===",
            format_tree_verbose(diff_ctx),
        ]
        debug_text = "\n".join(debug_lines) + "\n"
        debug_path = os.path.join(env["BUILD_LOCAL_PATH_BB"], "target", "obj", f"{file_stem}.acrt.debug")
        atomic_write(debug_path, debug_text)

    engine = RuleEngine(rules)

    master_findings = []
    local_findings = []
    diff_findings = []
    per_rule = {"MASTER": {}, "LOCAL": {}, "DIFF": {}}

    for rule in rules:
        if not rule.run:
            continue
        if rule.on_master:
            res = engine.run(rule, master_ctx, "MASTER", element_name, file_stem)
            master_findings.extend(res)
            per_rule["MASTER"][rule.number] = res
        if rule.on_local:
            res = engine.run(rule, local_ctx, "LOCAL", element_name, file_stem)
            local_findings.extend(res)
            per_rule["LOCAL"][rule.number] = res
        if rule.on_diff:
            res = engine.run(rule, diff_ctx, "DIFF", element_name, file_stem)
            diff_findings.extend(res)
            per_rule["DIFF"][rule.number] = res

    master_signatures = {f.signature() for f in master_findings}
    actionable = []
    for f in diff_findings:
        actionable.append(f)
    for f in local_findings:
        if f.signature() not in master_signatures:
            actionable.append(f)

    actionable = _sort_findings(actionable)

    counts_master = _counts_by_severity(master_findings)
    counts_local = _counts_by_severity(local_findings)
    counts_diff = _counts_by_severity(diff_findings)

    delta = {
        "E": counts_local["E"] - counts_master["E"],
        "W": counts_local["W"] - counts_master["W"],
        "I": counts_local["I"] - counts_master["I"],
    }

    exit_code = 0
    if delta["E"] > thresholds.errors or delta["W"] > thresholds.warnings or delta["I"] > thresholds.infos:
        exit_code = 1

    report_text = build_report(
        version=__version__,
        element=element_name,
        master_path=master_path,
        local_path=local_path,
        thresholds=thresholds,
        master_ctx=master_ctx,
        local_ctx=local_ctx,
        diff_ctx=diff_ctx,
        per_rule=per_rule,
        counts_master=counts_master,
        counts_local=counts_local,
        counts_diff=counts_diff,
        delta=delta,
        actionable=actionable,
        rules=rules,
    )

    report_path = os.path.join(env["BUILD_LOCAL_PATH_BB"], "target", "obj", f"{file_stem}.acrt")
    atomic_write(report_path, report_text)

    return RunResult(exit_code=exit_code, actionable=actionable, report_text=report_text)
