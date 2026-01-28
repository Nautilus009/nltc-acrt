import getpass
import re

from .rules import SEVERITY_LABELS
from .util import now_timestamp


_HEADER_LINE = "=" * 50
_SECTION_LINE = "-" * 50
_LONG_NUM_RE = re.compile(r"\d{8,}")
_QUOTE_RE = re.compile(r"\"[^\"]*\"|'[^']*'")


def build_report(
    version,
    element,
    master_path,
    local_path,
    thresholds,
    master_ctx,
    local_ctx,
    diff_ctx,
    per_rule,
    counts_master,
    counts_local,
    counts_diff,
    delta,
    actionable,
    rules,
):
    rules_by_number = {r.number: r for r in rules}
    timestamp = now_timestamp()
    user = getpass.getuser()
    lines = []
    lines.append(
        f"Getting current ACRT v{version} status for {element} on {timestamp} (user: {user}):"
    )
    lines.append(
        f"On Master build directory : #Errors {counts_master['E']} #Warnings {counts_master['W']} #Infos {counts_master['I']}"
    )
    lines.append(
        f"On Private build directory : #Errors {counts_local['E']} #Warnings {counts_local['W']} #Infos {counts_local['I']}"
    )
    lines.append(
        f"The difference : Errors {delta['E']} Warnings {delta['W']} Infos {delta['I']}"
    )
    lines.append("")
    lines.append("ACRT RULES REPORT")
    lines.append(_HEADER_LINE)

    def extra_attrs(rule):
        skip = {"number", "severity", "run", "type", "on_master", "on_local", "on_diff"}
        extras = {k: v for k, v in rule.attrs.items() if k not in skip}
        if not extras:
            return ""
        parts = [f"{k}={extras[k]}" for k in sorted(extras)]
        return f" ({','.join(parts)})"

    def first_rule_lines(rule, findings):
        if rule.type == "FORBIDDEN_LONG_NUMBERS":
            sample = [f"Hard-coded long numeric strings count={len(findings)}"]
            if findings:
                f = findings[0]
                literal = None
                if f.snippet:
                    m = _QUOTE_RE.search(f.snippet)
                    if m:
                        literal = m.group(0)
                    else:
                        m = _LONG_NUM_RE.search(f.snippet)
                        if m:
                            literal = m.group(0)
                if literal:
                    sample.append(f"Line {f.line}: {literal}")
            return sample
        if rule.type == "UNIQUE_PARAGRAPHS":
            sample = [f"Duplicate PARAGRAPH(s) count={len(findings)}"]
            if findings:
                name = findings[0].snippet.replace("Duplicate name:", "").strip()
                sample.append(f"Duplicate PARAGRAPH: {name} at line {findings[0].line}")
            return sample
        if rule.type == "PARAGRAPH_PREFIX_MATCH":
            sample = [f"PARAGRAPH prefix mismatch count={len(findings)}"]
            if findings:
                snippet = findings[0].snippet
                if " in SECTION " in snippet:
                    para, section = snippet.split(" in SECTION ", 1)
                    sample.append(f"{para} (line {findings[0].line}) is in SECTION {section}")
            return sample
        if rule.type == "GOTO_EXIT_ONLY_SAME_SECTION":
            sample = [f"GO TO violations count={len(findings)}"]
            if findings:
                f = findings[0]
                m = re.search(r"\bGO\s+TO\s+([A-Z0-9_-]+)", f.snippet or "")
                target = m.group(1) if m else "?"
                section = None
                parts = f.path.split("/") if f.path else []
                if len(parts) >= 2:
                    section = parts[1]
                if section:
                    prefix = section.split("-")[0] if "-" in section else section
                    allowed = [f"{prefix}-EX", f"{prefix}-EXIT"]
                    sample.append(
                        f"GO TO {target} at line {f.line}: GO TO allowed only to {allowed} in SECTION {section}"
                    )
            return sample
        if rule.type == "END_CLAUSE_REQUIRED":
            sample = [f"Missing END clause count={len(findings)}"]
            if findings:
                f = findings[0]
                starter = (f.snippet or "").split()[0] if f.snippet else "?"
                sample.append(f"{starter} started at line {f.line} missing END-{starter}")
            return sample
        if rule.type == "EVALUATE_WHEN_OTHER":
            sample = [f"EVALUATE without WHEN OTHER count={len(findings)}"]
            if findings:
                sample.append(f"EVALUATE at line {findings[0].line} missing WHEN OTHER")
            return sample
        if rule.type == "PROGRAM_ID_MATCH":
            sample = []
            if findings:
                expected = element.rsplit(".", 1)[0].upper()
                actual = findings[0].snippet.replace("PROGRAM-ID", "").strip()
                sample.append(f"PROGRAM-ID={actual} (expected {expected})")
            return sample
        if rule.type == "REGEX":
            return [f"    {findings[0].snippet}"] if findings else []
        return [findings[0].snippet] if findings else []

    def render_rule_block(rule_number, findings):
        rule = rules_by_number.get(rule_number)
        if not rule:
            return []
        sev = SEVERITY_LABELS.get(rule.severity, rule.severity)
        block = [f"[{rule.severity}] Rule {rule.number}: {rule.description}"]
        if rule.type == "REGEX":
            block.append(f"   Regex : {rule.code}")
        else:
            block.append(f"   Type  : {rule.type}{extra_attrs(rule)}")
        block.append(f"   Hits  : {len(findings)}")
        block.append("   Samples:")
        for line in first_rule_lines(rule, findings):
            block.append(f"     - {line}")
        for f in findings:
            line_no = "?" if f.line is None else f.line
            block.append(
                f"   {element}:{line_no}: {sev}: ACRT v{version} Rule {rule.number}: {rule.description}"
            )
        return block

    def render_target(title, target_key):
        lines = [f"Rule matches on {title}:", _SECTION_LINE]
        any_findings = False
        for rule_number, findings in per_rule[target_key].items():
            if not findings:
                continue
            any_findings = True
            lines.extend(render_rule_block(rule_number, findings))
            lines.append("")
        if not any_findings:
            lines.append("No rule matches.")
            lines.append("")
        return lines

    lines.extend(render_target("LOCAL LISTING (Private build)", "LOCAL"))
    lines.extend(render_target("MASTER LISTING (ALM build)", "MASTER"))
    lines.extend(render_target("LOCAL-ONLY DIFF (Private - Master)", "DIFF"))

    lines.append(
        f"Thresholds: Errors>{thresholds.errors} Warnings>{thresholds.warnings} Infos>{thresholds.infos}"
    )
    lines.append(
        f"Current abs diff: Errors={delta['E']} Warnings={delta['W']} Infos={delta['I']}"
    )
    lines.append("")
    status = "ERROR" if (
        delta["E"] > thresholds.errors
        or delta["W"] > thresholds.warnings
        or delta["I"] > thresholds.infos
    ) else "OK"
    lines.append(
        f"RESULT: {status} - the following {element} program does not meet ACRT v{version} standards."
    )
    lines.append(f"Report generated at: {timestamp}")

    return "\n".join(lines) + "\n"
