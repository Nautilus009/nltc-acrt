import re
import sys
from typing import Dict, List, Tuple

from cobol import (
    find_a_main_statement_violations,
    find_divisions_in_source,
    find_duplicate_paragraphs,
    find_duplicate_sections,
    find_evaluate_without_when_other,
    find_goto_exit_only_violations,
    find_long_numeric_literals,
    find_missing_end_clauses,
    find_paragraph_prefix_mismatches,
    find_program_id,
    find_procedure_division_line,
    find_recursive_call_violations,
    find_recursive_perform_calls,
    get_section_decls_from_source,
    normalize_name_for_compare,
)
from core import acrt_tag
from rules import Rule, RuleMatch


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
    expected_prog_base: str,
    cob_lines_with_linenos: List[Tuple[int, str]] = None,
    line_map: List[int] = None,
    max_samples_per_rule: int = 5,
) -> Tuple[Dict[str, int], List[RuleMatch]]:
    """
    Apply rules to the given cleaned lines in a given context (MASTER/LOCAL/DIFF).
    For source-level rules we use the COBOL source index for accuracy and line numbers.
    """
    counts = {"E": 0, "W": 0, "I": 0}
    matches: List[RuleMatch] = []

    def _map_src_locs(src_locs: List[int]) -> List[int]:
        if not line_map:
            return sorted(set([ln for ln in src_locs if ln]))
        mapped: List[int] = []
        for ln in src_locs:
            if not ln:
                continue
            if 1 <= ln <= len(line_map):
                cob_ln = line_map[ln - 1]
                if cob_ln:
                    mapped.append(cob_ln)
        return sorted(set(mapped))

    for rule in rules:
        if not rule.run:
            continue
        if not rule_runs_in_context(rule, context):
            continue

        # --------------------
        # PROGRAM_ID_MATCH (6.26)
        # --------------------
        if rule.rtype == "PROGRAM_ID_MATCH":
            if not cob_lines_with_linenos:
                continue

            prog_id, prog_ln = find_program_id(cob_lines_with_linenos)
            expected_norm = normalize_name_for_compare(expected_prog_base)
            found_norm = normalize_name_for_compare(prog_id)

            if not prog_id:
                counts[rule.severity] += 1
                samples = ["PROGRAM-ID not found"]
                matches.append(RuleMatch(rule=rule, count=1, sample_lines=samples, src_locations=_map_src_locs([1])))
            elif expected_norm != found_norm:
                counts[rule.severity] += 1
                samples = [f"PROGRAM-ID={prog_id} (expected {expected_prog_base})"]
                src_locs = [prog_ln] if prog_ln else [1]
                matches.append(RuleMatch(rule=rule, count=1, sample_lines=samples, src_locations=_map_src_locs(src_locs)))
            continue

        # --------------------
        # UNIQUE_PARAGRAPHS (3.22)
        # --------------------
        if rule.rtype == "UNIQUE_PARAGRAPHS":
            if not cob_lines_with_linenos:
                continue

            para_dups = find_duplicate_paragraphs(cob_lines_with_linenos)
            sec_dups = find_duplicate_sections(cob_lines_with_linenos)
            total = len(para_dups) + len(sec_dups)
            if total:
                counts[rule.severity] += total
                samples: List[str] = []
                if sec_dups:
                    samples.append(f"Duplicate SECTION(s) count={len(sec_dups)}")
                if para_dups:
                    samples.append(f"Duplicate PARAGRAPH(s) count={len(para_dups)}")
                combined = [(lnno, "SECTION", name) for lnno, name in sec_dups] + [
                    (lnno, "PARAGRAPH", name) for lnno, name in para_dups
                ]
                combined.sort(key=lambda item: item[0])
                for lnno, kind, name in combined[:max_samples_per_rule]:
                    samples.append(f"Duplicate {kind}: {name} at line {lnno}")
                src_locs = [lnno for lnno, _ in sec_dups] + [lnno for lnno, _ in para_dups]
                matches.append(RuleMatch(rule=rule, count=total, sample_lines=samples, src_locations=_map_src_locs(src_locs)))
            continue

        # --------------------
        # PARAGRAPH_PREFIX_MATCH (3.23)
        # --------------------
        if rule.rtype == "PARAGRAPH_PREFIX_MATCH":
            if not cob_lines_with_linenos:
                continue

            mism = find_paragraph_prefix_mismatches(cob_lines_with_linenos)
            if mism:
                counts[rule.severity] += len(mism)
                samples: List[str] = [f"PARAGRAPH prefix mismatch count={len(mism)}"]
                for lnno, pname, sec in mism[:max_samples_per_rule]:
                    samples.append(f"{pname} (line {lnno}) is in SECTION {sec}")
                src_locs = [lnno for lnno, _, _ in mism]
                matches.append(RuleMatch(rule=rule, count=len(mism), sample_lines=samples, src_locations=_map_src_locs(src_locs)))
            continue

        # --------------------
        # EVALUATE_WHEN_OTHER (8.1)
        # --------------------
        if rule.rtype == "EVALUATE_WHEN_OTHER":
            if not cob_lines_with_linenos:
                continue

            viols = find_evaluate_without_when_other(cob_lines_with_linenos)
            if viols:
                counts[rule.severity] += len(viols)
                samples: List[str] = [f"EVALUATE without WHEN OTHER count={len(viols)}"]
                for lnno in viols[:max_samples_per_rule]:
                    samples.append(f"EVALUATE at line {lnno} missing WHEN OTHER")
                matches.append(RuleMatch(rule=rule, count=len(viols), sample_lines=samples, src_locations=_map_src_locs(viols)))
            continue

        # --------------------
        # END_CLAUSE_REQUIRED (2.23)
        # --------------------
        if rule.rtype == "END_CLAUSE_REQUIRED":
            if not cob_lines_with_linenos:
                continue

            missing = find_missing_end_clauses(cob_lines_with_linenos)
            if missing:
                counts[rule.severity] += len(missing)
                samples: List[str] = [f"Missing END clause count={len(missing)}"]
                for lnno, kind in missing[:max_samples_per_rule]:
                    samples.append(f"{kind} started at line {lnno} missing END-{kind}")
                src_locs = [lnno for lnno, _ in missing]
                matches.append(RuleMatch(rule=rule, count=len(missing), sample_lines=samples, src_locations=_map_src_locs(src_locs)))
            continue

        # --------------------
        # A_MAIN_STATEMENTS_ONLY (8.4)
        # --------------------
        if rule.rtype == "A_MAIN_STATEMENTS_ONLY":
            if not cob_lines_with_linenos:
                continue

            viols = find_a_main_statement_violations(cob_lines_with_linenos)
            if viols:
                counts[rule.severity] += len(viols)
                samples: List[str] = [f"A-MAIN disallowed statements count={len(viols)}"]
                for lnno, text in viols[:max_samples_per_rule]:
                    samples.append(f"Line {lnno}: {text}")
                src_locs = [lnno for lnno, _ in viols]
                matches.append(RuleMatch(rule=rule, count=len(viols), sample_lines=samples, src_locations=_map_src_locs(src_locs)))
            continue

        # --------------------
        # NO_RECURSIVE_CALLS (2.20)
        # --------------------
        if rule.rtype == "NO_RECURSIVE_CALLS":
            if not cob_lines_with_linenos:
                continue

            perf_viols = find_recursive_perform_calls(cob_lines_with_linenos)
            call_viols = find_recursive_call_violations(cob_lines_with_linenos)
            total = len(perf_viols) + len(call_viols)

            if total:
                counts[rule.severity] += total
                samples: List[str] = [f"Recursive PERFORM/CALL count={total}"]
                for lnno, src, dst in perf_viols[:max_samples_per_rule]:
                    samples.append(f"PERFORM recursion: {src} -> {dst} at line {lnno}")
                if len(samples) < max_samples_per_rule + 1:
                    for lnno, target in call_viols[: max_samples_per_rule - (len(samples) - 1)]:
                        samples.append(f"CALL recursion: {target} at line {lnno}")
                src_locs = [lnno for lnno, _, _ in perf_viols] + [lnno for lnno, _ in call_viols]
                matches.append(RuleMatch(rule=rule, count=total, sample_lines=samples, src_locations=_map_src_locs(src_locs)))
            continue

        # --------------------
        # FORBIDDEN_LONG_NUMBERS (100)
        # --------------------
        if rule.rtype == "FORBIDDEN_LONG_NUMBERS":
            if not cob_lines_with_linenos:
                continue

            viols = find_long_numeric_literals(cob_lines_with_linenos, min_len=8)
            if viols:
                counts[rule.severity] += len(viols)
                samples: List[str] = [f"Hard-coded long numeric strings count={len(viols)}"]
                for lnno, lit in viols[:max_samples_per_rule]:
                    samples.append(f"Line {lnno}: \"{lit}\"")
                src_locs = [lnno for lnno, _ in viols]
                matches.append(RuleMatch(rule=rule, count=len(viols), sample_lines=samples, src_locations=_map_src_locs(src_locs)))
            continue

        # --------------------
        # MAX_SECTIONS (5.32)
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

                matches.append(RuleMatch(rule=rule, count=1, sample_lines=samples, src_locations=_map_src_locs(src_locs)))
            continue

        # --------------------
        # REQUIRED_SECTIONS (2.22)
        # --------------------
        if rule.rtype == "REQUIRED_SECTIONS":
            if not cob_lines_with_linenos:
                continue

            decls = get_section_decls_from_source(cob_lines_with_linenos)
            present = {nm for nm, _ in decls}
            required = set((rule.required_list or []))

            missing = sorted([x for x in required if x not in present])
            if missing:
                counts[rule.severity] += 1
                proc_ln = find_procedure_division_line(cob_lines_with_linenos)
                samples = [f"Missing SECTION(s): {', '.join(missing)}"]
                src_locs = [proc_ln]
                matches.append(RuleMatch(rule=rule, count=1, sample_lines=samples, src_locations=_map_src_locs(src_locs)))
            continue

        # --------------------
        # REQUIRED_DIVISIONS (3.19)
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
                anchor = divs.get("IDENTIFICATION", 1)
                samples = [f"Missing DIVISION(s): {', '.join(missing)}"]
                src_locs = [anchor]
                matches.append(RuleMatch(rule=rule, count=1, sample_lines=samples, src_locations=_map_src_locs(src_locs)))
            continue

        # --------------------
        # UNUSED_SECTIONS (5.1)
        # --------------------
        if rule.rtype == "UNUSED_SECTIONS":
            if not cob_lines_with_linenos:
                continue

            decls = get_section_decls_from_source(cob_lines_with_linenos)
            declared = [(nm.upper(), lnno) for nm, lnno in decls]
            declared_names = {nm for nm, _ in declared}

            exclude = set((rule.exclude_list or []))

            ref_rx = re.compile(
                r"\bPERFORM\s+([A-Z0-9][A-Z0-9-]*)\b|\bGO\s+TO\s+([A-Z0-9][A-Z0-9-]*)\b|\bGOTO\s+([A-Z0-9][A-Z0-9-]*)\b",
                re.IGNORECASE,
            )

            referenced: Dict[str, List[int]] = {}
            for lnno, txt in cob_lines_with_linenos:
                t = re.sub(r"\*>.*$", "", txt)
                for m in ref_rx.finditer(t):
                    target = None
                    if m.group(1):
                        target = m.group(1)
                    elif m.group(2):
                        target = m.group(2)
                    elif m.group(3):
                        target = m.group(3)
                    if not target:
                        continue
                    target = target.upper()
                    if target in declared_names:
                        referenced.setdefault(target, []).append(lnno)

            unused: List[Tuple[str, int]] = []
            for nm, decl_ln in declared:
                if nm in exclude:
                    continue
                if nm not in referenced:
                    unused.append((nm, decl_ln))

            if unused:
                counts[rule.severity] += len(unused)
                samples: List[str] = [f"Unused SECTION(s) count={len(unused)}"]
                for nm, decl_ln in unused[:max_samples_per_rule]:
                    samples.append(f"{nm} (declared at line {decl_ln})")
                src_locs = [decl_ln for _, decl_ln in unused]
                matches.append(RuleMatch(rule=rule, count=len(unused), sample_lines=samples, src_locations=_map_src_locs(src_locs)))
            continue

        # --------------------
        # GOTO_EXIT_ONLY_SAME_SECTION (8.3 tightened)
        # --------------------
        if rule.rtype == "GOTO_EXIT_ONLY_SAME_SECTION":
            if not cob_lines_with_linenos:
                continue

            viols = find_goto_exit_only_violations(cob_lines_with_linenos)
            if viols:
                counts[rule.severity] += len(viols)
                samples: List[str] = [f"GO TO violations count={len(viols)}"]
                for v_ln, target, reason in viols[:max_samples_per_rule]:
                    samples.append(f"GO TO {target} at line {v_ln}: {reason}")
                src_locs = [v_ln for v_ln, _, _ in viols]
                matches.append(RuleMatch(rule=rule, count=len(viols), sample_lines=samples, src_locations=_map_src_locs(src_locs)))
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
                    src_locations=_map_src_locs(src_locs),
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
        elif m.rule.rtype == "UNUSED_SECTIONS":
            out.append(f"   Type  : UNUSED_SECTIONS (exclude={','.join(m.rule.exclude_list or [])})\n")
        elif m.rule.rtype == "GOTO_EXIT_ONLY_SAME_SECTION":
            out.append("   Type  : GOTO_EXIT_ONLY_SAME_SECTION\n")
        elif m.rule.rtype == "PROGRAM_ID_MATCH":
            out.append("   Type  : PROGRAM_ID_MATCH\n")
        elif m.rule.rtype == "UNIQUE_PARAGRAPHS":
            out.append("   Type  : UNIQUE_PARAGRAPHS\n")
        elif m.rule.rtype == "PARAGRAPH_PREFIX_MATCH":
            out.append("   Type  : PARAGRAPH_PREFIX_MATCH\n")
        elif m.rule.rtype == "EVALUATE_WHEN_OTHER":
            out.append("   Type  : EVALUATE_WHEN_OTHER\n")
        elif m.rule.rtype == "END_CLAUSE_REQUIRED":
            out.append("   Type  : END_CLAUSE_REQUIRED\n")
        elif m.rule.rtype == "A_MAIN_STATEMENTS_ONLY":
            out.append("   Type  : A_MAIN_STATEMENTS_ONLY\n")
        elif m.rule.rtype == "NO_RECURSIVE_CALLS":
            out.append("   Type  : NO_RECURSIVE_CALLS\n")
        elif m.rule.rtype == "FORBIDDEN_LONG_NUMBERS":
            out.append("   Type  : FORBIDDEN_LONG_NUMBERS\n")
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
                out.append(f"   {cob_filename}:{lnno}: {sw}: {acrt_tag()} Rule {m.rule.number}: {m.rule.description}\n")
        else:
            out.append(f"   {cob_filename}:?: {sw}: {acrt_tag()} Rule {m.rule.number}: {m.rule.description} (line not found)\n")

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
                out.append(f"   {cob_filename}:{lnno}: {sw}: {acrt_tag()} Rule {m.rule.number}: {m.rule.description}")
        else:
            out.append(f"   {cob_filename}:?: {sw}: {acrt_tag()} Rule {m.rule.number}: {m.rule.description} (line not found)")

    out = sorted(out)
    return "\n".join(out) + ("\n" if out else "")
