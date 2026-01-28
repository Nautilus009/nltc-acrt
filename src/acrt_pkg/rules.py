import re
from .listing import read_listing, clean_source
from .util import extract_program_id
from .tokenize import _is_inline_perform
from dataclasses import dataclass
from .tokenize import IDENT_TOKEN_RE


SEVERITY_LABELS = {"E": "Error", "W": "Warning", "I": "Info"}


@dataclass
class Finding:
    rule_number: str
    severity: str
    description: str
    rule_type: str
    target: str
    element: str
    line: object
    path: str
    snippet: str

    def signature(self):
        return f"{self.rule_number}|{self.path}|{self.line}|{self.snippet}"


class RuleEngine:
    def __init__(self, rules):
        self.rules = rules
        self._compiled = {}

    def _regex(self, rule):
        key = rule.code
        if key not in self._compiled:
            self._compiled[key] = re.compile(rule.code, re.IGNORECASE)
        return self._compiled[key]

    def run(self, rule, context, target, element_name, file_stem):
        if not getattr(context, "meta", {}).get("tokenized_once"):
            raise RuntimeError("Rule execution attempted without prebuilt tokenization context")
        handler = getattr(self, f"_run_{rule.type.lower()}", None)
        if not handler:
            return []
        return handler(rule, context, target, element_name, file_stem)

    def _find_program_id(self, context):
        for node in context.by_kind["COMMAND"]:
            if node.division == "IDENTIFICATION":
                program_id = extract_program_id(node.normalized_text)
                if program_id:
                    return program_id, node
        program_id = context.meta.get("source_program_id") if hasattr(context, "meta") else None
        program_line = context.meta.get("source_program_line") if hasattr(context, "meta") else None
        if program_id:
            return program_id, program_line
        return None, None

    def _command_nodes(self, context, scope="COMMAND"):
        return context.by_kind.get(scope, [])

    def _run_regex(self, rule, context, target, element_name, file_stem):
        pattern = self._regex(rule)
        findings = []
        for node in self._command_nodes(context):
            if pattern.search(node.normalized_text):
                findings.append(
                    Finding(
                        rule_number=rule.number,
                        severity=rule.severity,
                        description=rule.description,
                        rule_type=rule.type,
                        target=target,
                        element=element_name,
                        line=node.start_line,
                        path=node.path(),
                        snippet=node.normalized_text,
                    )
                )
        return findings

    def _run_forbidden_long_numbers(self, rule, context, target, element_name, file_stem):
        pattern = re.compile(r"\d{8,}")
        findings = []
        for node in self._command_nodes(context):
            if node.start_line is None:
                continue
            if pattern.search(node.normalized_text):
                findings.append(
                    Finding(
                        rule_number=rule.number,
                        severity=rule.severity,
                        description=rule.description,
                        rule_type=rule.type,
                        target=target,
                        element=element_name,
                        line=node.start_line,
                        path=node.path(),
                        snippet=node.normalized_text,
                    )
                )
        seen_lines = {f.line for f in findings if f.line is not None}
        for var in context.by_kind.get("VARIABLE", []):
            if not hasattr(var, "meta"):
                continue
            if var.start_line is None:
                continue
            if not var.meta.get("value_explicit"):
                continue
            value = var.meta.get("value", "")
            if not value:
                continue
            if pattern.search(value):
                if var.start_line in seen_lines:
                    continue
                findings.append(
                    Finding(
                        rule_number=rule.number,
                        severity=rule.severity,
                        description=rule.description,
                        rule_type=rule.type,
                        target=target,
                        element=element_name,
                        line=var.start_line,
                        path=var.path(),
                        snippet=f"{var.name} VALUE {value}",
                    )
                )
                if var.start_line is not None:
                    seen_lines.add(var.start_line)
        return findings

    def _run_required_divisions(self, rule, context, target, element_name, file_stem):
        required = rule.attrs.get("required", "")
        required_list = [r.strip().upper() for r in required.split(",") if r.strip()]
        existing = {d.name for d in context.by_kind["DIVISION"]}
        missing = [d for d in required_list if d not in existing]
        if not missing:
            return []
        line = context.by_kind["DIVISION"][0].start_line if context.by_kind["DIVISION"] else 1
        return [
            Finding(
                rule_number=rule.number,
                severity=rule.severity,
                description=rule.description,
                rule_type=rule.type,
                target=target,
                element=element_name,
                line=line,
                path="PROGRAM",
                snippet=f"Missing DIVISION(s): {', '.join(missing)}",
            )
        ]

    def _run_required_sections(self, rule, context, target, element_name, file_stem):
        required = rule.attrs.get("required", "")
        required_list = [r.strip().upper() for r in required.split(",") if r.strip()]
        sections = [s for s in context.by_kind["SECTION"] if s.division == "PROCEDURE"]
        existing = {s.name for s in sections}
        missing = [s for s in required_list if s not in existing]
        if not missing:
            return []
        proc_div = next((d for d in context.by_kind["DIVISION"] if d.name == "PROCEDURE"), None)
        line = proc_div.start_line if proc_div else (sections[0].start_line if sections else 1)
        return [
            Finding(
                rule_number=rule.number,
                severity=rule.severity,
                description=rule.description,
                rule_type=rule.type,
                target=target,
                element=element_name,
                line=line,
                path="PROCEDURE",
                snippet=f"Missing SECTION(s): {', '.join(missing)}",
            )
        ]

    def _run_unique_paragraphs(self, rule, context, target, element_name, file_stem):
        findings = []
        seen = {}
        for node in context.by_kind["SECTION"] + context.by_kind["PARAGRAPH"]:
            name = node.name
            if name in seen:
                findings.append(
                    Finding(
                        rule_number=rule.number,
                        severity=rule.severity,
                        description=rule.description,
                        rule_type=rule.type,
                        target=target,
                        element=element_name,
                        line=node.start_line,
                        path=node.path(),
                        snippet=f"Duplicate name: {name}",
                    )
                )
            else:
                seen[name] = node
        return findings

    def _run_paragraph_prefix_match(self, rule, context, target, element_name, file_stem):
        findings = []
        for para in context.by_kind["PARAGRAPH"]:
            if para.division != "PROCEDURE" or not para.section:
                continue
            section_name = para.section
            prefix = section_name.split("-")[0] if "-" in section_name else section_name
            if not para.name.startswith(prefix + "-"):
                findings.append(
                    Finding(
                        rule_number=rule.number,
                        severity=rule.severity,
                        description=rule.description,
                        rule_type=rule.type,
                        target=target,
                        element=element_name,
                        line=para.start_line,
                        path=para.path(),
                        snippet=f"{para.name} in SECTION {section_name}",
                    )
                )
        return findings

    def _run_program_id_match(self, rule, context, target, element_name, file_stem):
        program_id, node = self._find_program_id(context)
        if not program_id and hasattr(context, "meta"):
            program_id = context.meta.get("source_program_id")
            node = context.meta.get("source_program_line")
        if hasattr(context, "meta") and not context.meta.get("source_program_line"):
            source_path = context.meta.get("source_path")
            if source_path:
                source_raw = read_listing(source_path)
                for text, line_no, _c1 in clean_source(source_raw):
                    found = extract_program_id(text)
                    if found:
                        context.meta["source_program_id"] = found
                        context.meta["source_program_line"] = line_no
                        program_id = program_id or found
                        node = node or line_no
                        break
        if not program_id:
            return [
                Finding(
                    rule_number=rule.number,
                    severity=rule.severity,
                    description=rule.description,
                    rule_type=rule.type,
                    target=target,
                    element=element_name,
                    line=None,
                    path="IDENTIFICATION",
                    snippet="PROGRAM-ID not found",
                )
            ]
        if program_id.upper() != file_stem.upper():
            source_line = context.meta.get("source_program_line") if hasattr(context, "meta") else None
            line = source_line or (node.start_line if hasattr(node, "start_line") else None)
            path = node.path() if hasattr(node, "path") else "IDENTIFICATION"
            return [
                Finding(
                    rule_number=rule.number,
                    severity=rule.severity,
                    description=rule.description,
                    rule_type=rule.type,
                    target=target,
                    element=element_name,
                    line=line,
                    path=path,
                    snippet=f"PROGRAM-ID {program_id}",
                )
            ]
        return []

    def _run_evaluate_when_other(self, rule, context, target, element_name, file_stem):
        findings = []
        for node in context.by_kind["COMMAND"]:
            if node.meta.get("starter") == "EVALUATE":
                branches = node.meta.get("branches", set())
                if "WHEN OTHER" not in branches:
                    findings.append(
                        Finding(
                            rule_number=rule.number,
                            severity=rule.severity,
                            description=rule.description,
                            rule_type=rule.type,
                            target=target,
                            element=element_name,
                            line=node.start_line,
                            path=node.path(),
                            snippet=node.normalized_text,
                        )
                    )
        return findings

    def _run_end_clause_required(self, rule, context, target, element_name, file_stem):
        findings = []
        for node in context.by_kind["COMMAND"]:
            starter = node.meta.get("starter")
            if not starter:
                continue
            if starter not in {"IF", "EVALUATE", "READ", "PERFORM"}:
                continue
            if starter == "PERFORM":
                # Inline PERFORM should not require END-PERFORM.
                if _is_inline_perform(node.normalized_text):
                    continue
            if node.closed_by and node.closed_by.startswith("END-"):
                continue
            findings.append(
                Finding(
                    rule_number=rule.number,
                    severity=rule.severity,
                    description=rule.description,
                    rule_type=rule.type,
                    target=target,
                    element=element_name,
                    line=node.start_line,
                    path=node.path(),
                    snippet=node.normalized_text,
                )
            )
        return findings

    def _run_unused_sections(self, rule, context, target, element_name, file_stem):
        exclude = rule.attrs.get("exclude", "")
        exclude_set = {s.strip().upper() for s in exclude.split(",") if s.strip()}
        sections = [s for s in context.by_kind["SECTION"] if s.division == "PROCEDURE"]
        para_to_section = {p.name: p.section for p in context.by_kind["PARAGRAPH"]}
        referenced = set()
        for node in context.by_kind["COMMAND"]:
            if node.division != "PROCEDURE":
                continue
            text = node.normalized_text
            if "PERFORM" in node.keywords:
                m = re.search(r"\bPERFORM\s+([A-Z0-9_-]+)", text)
                if m:
                    target = m.group(1)
                    if target in para_to_section:
                        referenced.add(para_to_section[target])
                    referenced.add(target)
            if "GO" in node.keywords and "TO" in node.keywords:
                m = re.search(r"\bGO\s+TO\s+([A-Z0-9_-]+)", text)
                if m:
                    target = m.group(1)
                    if target in para_to_section:
                        referenced.add(para_to_section[target])
                    referenced.add(target)
        findings = []
        for section in sections:
            if section.name in exclude_set:
                continue
            if section.name not in referenced:
                findings.append(
                    Finding(
                        rule_number=rule.number,
                        severity=rule.severity,
                        description=rule.description,
                        rule_type=rule.type,
                        target=target,
                        element=element_name,
                        line=section.start_line,
                        path=section.path(),
                        snippet=f"Unused SECTION {section.name}",
                    )
                )
        return findings

    def _run_max_sections(self, rule, context, target, element_name, file_stem):
        max_value = int(rule.attrs.get("max", "0"))
        sections = [s for s in context.by_kind["SECTION"] if s.division == "PROCEDURE"]
        if len(sections) <= max_value:
            return []
        proc_div = next((d for d in context.by_kind["DIVISION"] if d.name == "PROCEDURE"), None)
        line = proc_div.start_line if proc_div else (sections[0].start_line if sections else 1)
        return [
            Finding(
                rule_number=rule.number,
                severity=rule.severity,
                description=rule.description,
                rule_type=rule.type,
                target=target,
                element=element_name,
                line=line,
                path="PROCEDURE",
                snippet=f"SECTION count={len(sections)} (max {max_value})",
            )
        ]

    def _run_a_main_statements_only(self, rule, context, target, element_name, file_stem):
        allowed = {"PERFORM", "EVALUATE", "IF", "GOBACK", "EXIT", "SORT"}
        findings = []
        for node in context.by_kind["COMMAND"]:
            if node.section != "A-MAIN":
                continue
            keyword = node.name
            if not keyword:
                continue
            if keyword == "EXIT":
                if not re.match(r"^EXIT(\s+PROGRAM)?\s*\.?$", node.normalized_text.strip()):
                    findings.append(
                        Finding(
                            rule_number=rule.number,
                            severity=rule.severity,
                            description=rule.description,
                            rule_type=rule.type,
                            target=target,
                            element=element_name,
                            line=node.start_line,
                            path=node.path(),
                            snippet=node.normalized_text,
                        )
                    )
                continue
            if keyword not in allowed:
                findings.append(
                    Finding(
                        rule_number=rule.number,
                        severity=rule.severity,
                        description=rule.description,
                        rule_type=rule.type,
                        target=target,
                        element=element_name,
                        line=node.start_line,
                        path=node.path(),
                        snippet=node.normalized_text,
                    )
                )
        return findings

    def _run_goto_exit_only_same_section(self, rule, context, target, element_name, file_stem):
        paragraphs = {p.name: p for p in context.by_kind["PARAGRAPH"]}
        findings = []
        for node in context.by_kind["COMMAND"]:
            if node.division != "PROCEDURE":
                continue
            if "GO" not in node.keywords or "TO" not in node.keywords:
                continue
            m = re.search(r"\bGO\s+TO\s+([A-Z0-9_-]+)", node.normalized_text)
            if not m:
                continue
            target_name = m.group(1)
            section = node.section
            if not section:
                findings.append(
                    Finding(
                        rule_number=rule.number,
                        severity=rule.severity,
                        description=rule.description,
                        rule_type=rule.type,
                        target=target,
                        element=element_name,
                        line=node.start_line,
                        path=node.path(),
                        snippet=node.normalized_text,
                    )
                )
                continue
            prefix = section.split("-")[0] if "-" in section else section
            allowed = {f"{prefix}-EX", f"{prefix}-EXIT"}
            para = paragraphs.get(target_name)
            if target_name not in allowed:
                findings.append(
                    Finding(
                        rule_number=rule.number,
                        severity=rule.severity,
                        description=rule.description,
                        rule_type=rule.type,
                        target=target,
                        element=element_name,
                        line=node.start_line,
                        path=node.path(),
                        snippet=node.normalized_text,
                    )
                )
                continue
            if not para:
                findings.append(
                    Finding(
                        rule_number=rule.number,
                        severity=rule.severity,
                        description=rule.description,
                        rule_type=rule.type,
                        target=target,
                        element=element_name,
                        line=node.start_line,
                        path=node.path(),
                        snippet=node.normalized_text,
                    )
                )
                continue
            has_only_exit = True
            for cmd in para.children:
                if cmd.kind != "COMMAND":
                    continue
                if not re.match(r"^EXIT(\s+PROGRAM)?\s*\.?$", cmd.normalized_text.strip()):
                    has_only_exit = False
                    break
            if not has_only_exit:
                findings.append(
                    Finding(
                        rule_number=rule.number,
                        severity=rule.severity,
                        description=rule.description,
                        rule_type=rule.type,
                        target=target,
                        element=element_name,
                        line=node.start_line,
                        path=node.path(),
                        snippet=node.normalized_text,
                    )
                )
        return findings

    def _run_no_recursive_calls(self, rule, context, target, element_name, file_stem):
        findings = []
        program_id, _ = self._find_program_id(context)
        program_id = (program_id or file_stem).upper()
        for para in context.by_kind["PARAGRAPH"]:
            for cmd in para.children:
                if cmd.kind != "COMMAND":
                    continue
                if "PERFORM" in cmd.keywords:
                    m = re.search(r"\bPERFORM\s+([A-Z0-9_-]+)", cmd.normalized_text)
                    if m and m.group(1) == para.name:
                        findings.append(
                            Finding(
                                rule_number=rule.number,
                                severity=rule.severity,
                                description=rule.description,
                                rule_type=rule.type,
                                target=target,
                                element=element_name,
                                line=cmd.start_line,
                                path=cmd.path(),
                                snippet=cmd.normalized_text,
                            )
                        )
                if "CALL" in cmd.keywords:
                    m = re.search(r"\bCALL\s+['\"]?([A-Z0-9_-]+)['\"]?", cmd.normalized_text)
                    if m and m.group(1).upper() == program_id:
                        findings.append(
                            Finding(
                                rule_number=rule.number,
                                severity=rule.severity,
                                description=rule.description,
                                rule_type=rule.type,
                                target=target,
                                element=element_name,
                                line=cmd.start_line,
                                path=cmd.path(),
                                snippet=cmd.normalized_text,
                            )
                        )
        return findings
