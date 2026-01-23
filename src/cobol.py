import re
from dataclasses import dataclass
from typing import Dict, List, Tuple


# Heuristic filter: never treat COBOL reserved words / terminators as paragraph labels.
# This prevents false positives like END-IF., END-EVALUATE., etc.
COBOL_RESERVED_LABELS = {
    # Common statement/verb keywords
    "ACCEPT", "ADD", "ALTER", "CALL", "CANCEL", "CLOSE", "COMMIT", "COMPUTE", "CONTINUE",
    "DELETE", "DISPLAY", "DIVIDE", "EVALUATE", "EXEC", "EXIT", "GOBACK", "GO", "GOTO",
    "IF", "INITIALIZE", "INSPECT", "INVOKE", "MERGE", "MOVE", "MULTIPLY", "NEXT", "OPEN",
    "PERFORM", "READ", "RELEASE", "RETURN", "REWRITE", "ROLLBACK", "SEARCH", "SET", "SORT",
    "START", "STOP", "STRING", "SUBTRACT", "UNSTRING", "WRITE",

    # Clauses / common tokens that appear at line starts
    "ELSE", "END", "THEN", "WHEN", "OTHER", "THRU", "THROUGH", "VARYING", "UNTIL",
    "DEPENDING", "ON", "GIVING", "USING", "FROM", "INTO", "WITH", "BY",

    # Divisions/sections that could appear in column 1
    "IDENTIFICATION", "ENVIRONMENT", "DATA", "PROCEDURE", "WORKING-STORAGE", "LINKAGE",
}

# Explicit END-* terminators that often look like labels in free format.
END_TERMINATORS_RX = re.compile(
    r"^(END-(IF|EVALUATE|READ|WRITE|REWRITE|PERFORM|CALL|SEARCH|START|STRING|UNSTRING|COMPUTE|"
    r"ADD|SUBTRACT|MULTIPLY|DIVIDE|DELETE|OPEN|CLOSE|SORT|MERGE))$",
    re.IGNORECASE,
)

NAME_TOKEN = r"[A-Z0-9][A-Z0-9_-]*"
SECTION_DECL_RX = re.compile(rf"^\s*({NAME_TOKEN})\s+SECTION\s*\.", re.IGNORECASE)
PARAGRAPH_LABEL_RX = re.compile(rf"^\s*({NAME_TOKEN})\s*\.(?:\s+.*)?$", re.IGNORECASE)
PERFORM_GOTO_REF_RX = re.compile(
    rf"\bPERFORM\s+({NAME_TOKEN})\b|\bGO\s+TO\s+({NAME_TOKEN})\b|\bGOTO\s+({NAME_TOKEN})\b",
    re.IGNORECASE,
)
DIVISION_DECL_RX = re.compile(r"^\s*([A-Z0-9-]+)\s+DIVISION\b", re.IGNORECASE)
COMMAND_TOKEN_RX = re.compile(r"^\s*([A-Z0-9-]+)\b", re.IGNORECASE)


@dataclass
class CobolLine:
    line_no: int
    text: str
    clean: str
    is_section: bool
    is_division: bool
    is_paragraph: bool
    command: str


@dataclass
class CobolComponents:
    lines: List[CobolLine]


def _clean_line(txt: str) -> str:
    return re.sub(r"\*>.*$", "", txt).rstrip()


def _strip_string_literals(txt: str) -> str:
    return re.sub(r"\"[^\"]*\"|'[^']*'", " ", txt)

def parse_cobol_components(cob_lines_with_linenos: List[Tuple[int, str]]) -> CobolComponents:
    lines: List[CobolLine] = []
    prev_sig_ends_with_dot = False
    for lnno, txt in cob_lines_with_linenos:
        clean = _clean_line(txt)
        if not clean or clean.strip() == "":
            continue

        is_div = DIVISION_DECL_RX.search(clean) is not None
        is_sec = SECTION_DECL_RX.search(clean) is not None
        is_par = False
        if not is_div and not is_sec:
            m = PARAGRAPH_LABEL_RX.match(clean)
            if m:
                name = (m.group(1) or "").upper()
                if not is_reserved_paragraph_label(name) and prev_sig_ends_with_dot:
                    is_par = True

        cmd = ""
        if not is_div and not is_sec and not is_par:
            cmd_text = _strip_string_literals(clean)
            m = COMMAND_TOKEN_RX.search(cmd_text)
            if m:
                cmd = (m.group(1) or "").upper()

        lines.append(
            CobolLine(
                line_no=lnno,
                text=txt,
                clean=clean,
                is_section=is_sec,
                is_division=is_div,
                is_paragraph=is_par,
                command=cmd,
            )
        )
        prev_sig_ends_with_dot = clean.strip().endswith(".")
    return CobolComponents(lines=lines)


def is_executable_line(cob_line: CobolLine) -> bool:
    return not (cob_line.is_section or cob_line.is_division or cob_line.is_paragraph)


def is_reserved_paragraph_label(name: str) -> bool:
    if not name:
        return False
    n = name.upper()
    if n in COBOL_RESERVED_LABELS:
        return True
    if END_TERMINATORS_RX.match(n) is not None:
        return True
    return False


def get_section_decls_from_source(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[str, int]]:
    """
    Return section declarations in source order as (SECTION-NAME, line_number),
    excluding WORKING-STORAGE and LINKAGE.
    """
    out: List[Tuple[str, int]] = []
    for lnno, txt in cob_lines_with_linenos:
        t = _clean_line(txt)
        m = SECTION_DECL_RX.search(t)
        if not m:
            continue
        name = (m.group(1) or "").upper()
        if name in ("WORKING-STORAGE", "LINKAGE"):
            continue
        out.append((name, lnno))
    return out


def get_section_decls_after_procedure_excluding_declaratives(
    cob_lines_with_linenos: List[Tuple[int, str]]
) -> List[Tuple[str, int]]:
    """
    Return section declarations after PROCEDURE DIVISION,
    excluding any sections inside DECLARATIVES.
    """
    proc_rx = re.compile(r"\bPROCEDURE\s+DIVISION\b", re.IGNORECASE)
    decl_start_rx = re.compile(r"^\s*DECLARATIVES\s*\.", re.IGNORECASE)
    decl_end_rx = re.compile(r"^\s*END\s+DECLARATIVES\s*\.", re.IGNORECASE)

    in_proc = False
    in_decl = False
    out: List[Tuple[str, int]] = []

    for lnno, txt in cob_lines_with_linenos:
        t = _clean_line(txt)
        if not t:
            continue

        if not in_proc and proc_rx.search(t):
            in_proc = True
            continue

        if not in_proc:
            continue

        if decl_start_rx.match(t):
            in_decl = True
            continue
        if decl_end_rx.match(t):
            in_decl = False
            continue

        if in_decl:
            continue

        m = SECTION_DECL_RX.search(t)
        if not m:
            continue
        name = (m.group(1) or "").upper()
        if name in ("WORKING-STORAGE", "LINKAGE"):
            continue
        out.append((name, lnno))

    return out


# ---------------------------
# GO TO rule helpers
# ---------------------------

def build_section_map(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str]]:
    """Return a list of (line_number, SECTION-NAME) for each SECTION declaration (excluding WORKING-STORAGE/LINKAGE)."""
    out: List[Tuple[int, str]] = []
    for lnno, txt in cob_lines_with_linenos:
        t = _clean_line(txt)
        m = SECTION_DECL_RX.search(t)
        if not m:
            continue
        name = (m.group(1) or "").upper()
        if name in ("WORKING-STORAGE", "LINKAGE"):
            continue
        out.append((lnno, name))
    return out


def current_section_at_line(section_decls: List[Tuple[int, str]], line_no: int) -> str:
    """Return the most recent SECTION name at or before line_no."""
    cur = ""
    for lnno, sec in section_decls:
        if lnno <= line_no:
            cur = sec
        else:
            break
    return cur


def section_prefix(section_name: str) -> str:
    """Return section prefix (text before the first '-')."""
    if not section_name:
        return ""
    return section_name.split("-", 1)[0].upper()


def build_paragraph_index(cob_lines_with_linenos: List[Tuple[int, str]]) -> Dict[str, Tuple[str, int]]:
    """Map PARAGRAPH-NAME -> (SECTION-NAME, decl_line_no)."""
    section_decls = build_section_map(cob_lines_with_linenos)

    idx: Dict[str, Tuple[str, int]] = {}
    for lnno, name in get_paragraph_decls(cob_lines_with_linenos):
        sec = current_section_at_line(section_decls, lnno)
        if name not in idx:
            idx[name] = (sec, lnno)

    return idx


def get_paragraph_decls(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str]]:
    """Return paragraph declarations as (line_number, paragraph_name)."""
    decls: List[Tuple[int, str]] = []
    components = parse_cobol_components(cob_lines_with_linenos)
    for line in components.lines:
        if not line.is_paragraph:
            continue
        m = PARAGRAPH_LABEL_RX.match(line.clean)
        if not m:
            continue
        name = (m.group(1) or "").upper()
        decls.append((line.line_no, name))
    return decls


def current_paragraph_at_line(paragraph_decls: List[Tuple[int, str]], line_no: int) -> str:
    """Return the most recent paragraph name at or before line_no."""
    cur = ""
    for lnno, name in paragraph_decls:
        if lnno <= line_no:
            cur = name
        else:
            break
    return cur


def get_paragraph_body_lines(cob_lines_with_linenos: List[Tuple[int, str]], paragraph_name: str) -> Tuple[int, List[str]]:
    """
    Return (paragraph_decl_line, body_lines) where body_lines are cleaned (no comments/blank),
    starting after the paragraph label line until the next paragraph/section/division label.

    If paragraph is not found, returns (0, []).
    """
    paragraph_name = (paragraph_name or "").upper()
    label_rx = re.compile(r"^\s*" + re.escape(paragraph_name) + r"\s*\.(.*)$", re.IGNORECASE)

    decls = get_paragraph_decls(cob_lines_with_linenos)
    decl_ln = 0
    for lnno, name in decls:
        if name == paragraph_name:
            decl_ln = lnno
            break
    if decl_ln == 0:
        return 0, []

    line_map = {lnno: txt for lnno, txt in cob_lines_with_linenos}
    decl_text = _clean_line(line_map.get(decl_ln, ""))
    body: List[str] = []

    m = label_rx.match(decl_text)
    if m:
        rest = (m.group(1) or "").strip()
        if rest:
            body.append(rest)

    paragraph_lines = {lnno for lnno, _name in decls}
    for lnno, txt in cob_lines_with_linenos:
        if lnno <= decl_ln:
            continue
        t = _clean_line(txt)
        if not t:
            continue
        if lnno in paragraph_lines or re.search(r"\bSECTION\b", t, re.IGNORECASE) or re.search(r"\bDIVISION\b", t, re.IGNORECASE):
            break
        body.append(t.strip())

    return decl_ln, body


def paragraph_is_exit_only(cob_lines_with_linenos: List[Tuple[int, str]], paragraph_name: str) -> Tuple[bool, int, str]:
    """
    Validate that the paragraph contains ONLY an EXIT statement.
    Acceptable forms (case-insensitive):
      - EXIT.
      - EXIT PARAGRAPH.

    Returns (ok, paragraph_decl_line, reason).
    """
    decl_ln, body = get_paragraph_body_lines(cob_lines_with_linenos, paragraph_name)
    if decl_ln == 0:
        return False, 0, "exit paragraph not found"

    if len(body) != 1:
        return False, decl_ln, f"exit paragraph must contain only EXIT statement (found {len(body)} statement line(s))"

    stmt = body[0].strip()
    if re.match(r"^EXIT(\s+PARAGRAPH)?\s*\.$", stmt, re.IGNORECASE) is None:
        return False, decl_ln, f"exit paragraph must contain only EXIT. or EXIT PARAGRAPH. (found: {stmt})"

    return True, decl_ln, ""


def find_goto_exit_only_violations(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str, str]]:
    """
    Tightened rule:
      - GO TO is permitted only to the EXIT paragraph of the same SECTION.
      - The EXIT paragraph itself must contain ONLY an EXIT statement.

    Allowed targets inside SECTION <SEC> are:
      <PREFIX>-EX
      <PREFIX>-EXIT
    Where <PREFIX> is the part of the SECTION name up to the first '-'.

    We check only simple GO TO/GOTO with a single target label (no DEPENDING ON).
    """
    section_decls = build_section_map(cob_lines_with_linenos)
    par_idx = build_paragraph_index(cob_lines_with_linenos)

    goto_rx = re.compile(
        rf"\bGO\s+TO\s+({NAME_TOKEN})\b|\bGOTO\s+({NAME_TOKEN})\b",
        re.IGNORECASE,
    )
    dep_rx = re.compile(r"\bDEPENDING\s+ON\b", re.IGNORECASE)

    viols: List[Tuple[int, str, str]] = []

    for lnno, txt in cob_lines_with_linenos:
        t = re.sub(r"\*>.*$", "", txt)

        if dep_rx.search(t):
            continue

        m = goto_rx.search(t)
        if not m:
            continue

        target = (m.group(1) or m.group(2) or "").upper()
        if not target:
            continue

        cur_sec = current_section_at_line(section_decls, lnno)
        if not cur_sec:
            continue

        pref = section_prefix(cur_sec)
        allowed = {f"{pref}-EX", f"{pref}-EXIT"}

        if target not in par_idx:
            continue

        tgt_sec, _tgt_ln = par_idx[target]

        if cur_sec != tgt_sec:
            viols.append((lnno, target, f"target is in a different SECTION ({tgt_sec or 'UNKNOWN'})"))
            continue

        if target not in allowed:
            viols.append((lnno, target, f"GO TO allowed only to {sorted(allowed)} in SECTION {cur_sec}"))
            continue

        ok_exit, exit_decl_ln, reason = paragraph_is_exit_only(cob_lines_with_linenos, target)
        if not ok_exit:
            viols.append((exit_decl_ln or lnno, target, reason))
            continue

    return viols


# ---------------------------
# Next 5 rule helpers (source-level)
# ---------------------------

def normalize_name_for_compare(name: str) -> str:
    """Normalize names for comparison: remove non-alnum; treat '-' and '_' equivalently."""
    if not name:
        return ""
    n = name.upper().replace("_", "-")
    n = re.sub(r"[^A-Z0-9-]", "", n)
    n = n.replace("-", "")
    return n


def find_program_id(cob_lines_with_linenos: List[Tuple[int, str]]) -> Tuple[str, int]:
    """Return (PROGRAM-ID, line_no). If not found, returns ('', 0)."""
    rx = re.compile(r"\bPROGRAM-ID\s*\.\s*([A-Z0-9][A-Z0-9_-]*)", re.IGNORECASE)
    for lnno, txt in cob_lines_with_linenos:
        t = re.sub(r"\*>.*$", "", txt)
        m = rx.search(t)
        if m:
            return (m.group(1) or "").upper(), lnno
    return "", 0


def find_missing_end_clauses(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str]]:
    """
    Heuristic: require END-IF/END-EVALUATE/END-READ/END-PERFORM for inline PERFORM.
    Returns list of (line_no, construct) for unclosed constructs.
    """
    token_rx = re.compile(
        r"\b(END-IF|END-EVALUATE|END-READ|END-PERFORM|IF|EVALUATE|READ|PERFORM)\b",
        re.IGNORECASE,
    )
    inline_hint_rx = re.compile(r"\b(VARYING|UNTIL|TEST|AFTER|BEFORE|WITH)\b", re.IGNORECASE)
    perform_target_rx = re.compile(rf"\bPERFORM\s+({NAME_TOKEN})\b", re.IGNORECASE)

    components = parse_cobol_components(cob_lines_with_linenos)
    stack: List[Tuple[str, int]] = []
    lines = components.lines

    def _next_significant_line(start_idx: int) -> str:
        for _i in range(start_idx + 1, len(lines)):
            _txt = lines[_i].clean.strip()
            if not _txt:
                continue
            return _txt
        return ""

    next_target_rx = re.compile(rf"^\s*({NAME_TOKEN})\b", re.IGNORECASE)

    def _is_standalone_token(line: str, match: re.Match) -> bool:
        start, end = match.start(1), match.end(1)
        prev = line[start - 1] if start > 0 else " "
        nxt = line[end] if end < len(line) else " "
        if prev.isalnum() or prev in "-_":
            return False
        if nxt.isalnum() or nxt in "-_":
            return False
        return True

    for _idx, line in enumerate(lines):
        t = _strip_string_literals(line.clean)
        if not t.strip():
            continue
        if not is_executable_line(line):
            continue

        for m in token_rx.finditer(t):
            if not _is_standalone_token(t, m):
                continue
            token = (m.group(1) or "").upper()
            if token.startswith("END-"):
                kind = token[4:]
                for i in range(len(stack) - 1, -1, -1):
                    if stack[i][0] == kind:
                        stack.pop(i)
                        break
                continue

            if token == "PERFORM":
                tgt = perform_target_rx.search(t)
                if not tgt:
                    nxt = _next_significant_line(_idx)
                    nxt_target = next_target_rx.search(nxt) if nxt else None
                    if nxt_target:
                        continue
                if not tgt and inline_hint_rx.search(t):
                    stack.append(("PERFORM", line.line_no))
                elif not tgt:
                    stack.append(("PERFORM", line.line_no))
                continue

            stack.append((token, line.line_no))

    return [(lnno, kind) for kind, lnno in stack]


def find_a_main_statement_violations(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str]]:
    """Return list of (line_no, line_text) for statements not allowed inside A-MAIN SECTION."""
    section_decls = build_section_map(cob_lines_with_linenos)
    allowed_tokens = {
        "PERFORM",
        "EVALUATE",
        "IF",
        "GOBACK",
        "SORT",
        "EXIT",
        "END-IF",
        "END-EVALUATE",
        "END-PERFORM",
        "ELSE",
        "WHEN",
    }
    components = parse_cobol_components(cob_lines_with_linenos)

    violations: List[Tuple[int, str]] = []
    for line in components.lines:
        t = line.clean
        t_cmd = _strip_string_literals(t)
        if not t_cmd.strip():
            continue
        if not is_executable_line(line):
            continue

        cur_sec = current_section_at_line(section_decls, line.line_no)
        if cur_sec != "A-MAIN":
            continue

        token = line.command
        if not token:
            continue
        if token in allowed_tokens:
            continue
        if token in COBOL_RESERVED_LABELS:
            violations.append((line.line_no, t.strip()))

    return violations


def find_recursive_perform_calls(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str, str]]:
    """Return list of (line_no, src_paragraph, target_paragraph) where a PERFORM is recursive."""
    par_decls = get_paragraph_decls(cob_lines_with_linenos)
    par_idx = {name: lnno for lnno, name in par_decls}
    perform_rx = re.compile(rf"\bPERFORM\s+({NAME_TOKEN})\b", re.IGNORECASE)
    components = parse_cobol_components(cob_lines_with_linenos)

    edges: List[Tuple[str, str, int]] = []
    for line in components.lines:
        t = _strip_string_literals(line.clean)
        if line.is_paragraph:
            continue
        src = current_paragraph_at_line(par_decls, line.line_no)
        if not src:
            continue
        for m in perform_rx.finditer(t):
            target = (m.group(1) or "").upper()
            if target in par_idx:
                edges.append((src, target, line.line_no))

    graph: Dict[str, List[str]] = {}
    for src, dst, _lnno in edges:
        graph.setdefault(src, []).append(dst)

    reachable: Dict[str, set] = {}

    def dfs(node: str, visiting: set) -> set:
        if node in reachable:
            return reachable[node]
        visiting.add(node)
        out = set()
        for nxt in graph.get(node, []):
            out.add(nxt)
            if nxt not in visiting:
                out |= dfs(nxt, visiting)
        visiting.remove(node)
        reachable[node] = out
        return out

    for node in graph:
        dfs(node, set())

    viols: List[Tuple[int, str, str]] = []
    for src, dst, lnno in edges:
        if src == dst:
            viols.append((lnno, src, dst))
            continue
        if src in reachable.get(dst, set()):
            viols.append((lnno, src, dst))

    return viols


def find_recursive_call_violations(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str]]:
    """Return list of (line_no, call_target) for CALLs that invoke the same PROGRAM-ID."""
    prog_id, _prog_ln = find_program_id(cob_lines_with_linenos)
    if not prog_id:
        return []

    call_lit_rx = re.compile(rf"\bCALL\s+['\"]({NAME_TOKEN})['\"]", re.IGNORECASE)
    viols: List[Tuple[int, str]] = []
    components = parse_cobol_components(cob_lines_with_linenos)

    for line in components.lines:
        t = line.clean
        for m in call_lit_rx.finditer(t):
            target = (m.group(1) or "").upper()
            if normalize_name_for_compare(target) == normalize_name_for_compare(prog_id):
                viols.append((line.line_no, target))

    return viols


def find_long_numeric_literals(cob_lines_with_linenos: List[Tuple[int, str]], min_len: int = 8) -> List[Tuple[int, str]]:
    """Return list of (line_no, literal_snippet) for string literals containing long digit sequences."""
    lit_rx = re.compile(r"(['\"])(.*?)\1")
    digits_rx = re.compile(r"\d+")
    viols: List[Tuple[int, str]] = []

    for lnno, txt in cob_lines_with_linenos:
        t = re.sub(r"\*>.*$", "", txt)
        for m in lit_rx.finditer(t):
            lit = m.group(2) or ""
            for dm in digits_rx.finditer(lit):
                seq = dm.group(0)
                if len(seq.lstrip("0")) >= min_len:
                    viols.append((lnno, lit))
                    break

    return viols


def build_paragraph_occurrences(cob_lines_with_linenos: List[Tuple[int, str]]) -> Dict[str, List[int]]:
    """Return paragraph name -> list of declaration line numbers (all occurrences)."""
    section_decls = build_section_map(cob_lines_with_linenos)
    occ: Dict[str, List[int]] = {}
    for lnno, name in get_paragraph_decls(cob_lines_with_linenos):
        # Reduce false positives: in our conventions paragraph labels include '-' or '_'
        if "-" not in name and "_" not in name:
            continue
        if not current_section_at_line(section_decls, lnno):
            continue
        occ.setdefault(name, []).append(lnno)
    return occ


def find_duplicate_paragraphs(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str]]:
    """Return list of (line_no, paragraph_name) for duplicate paragraph declarations (excluding first occurrence)."""
    occ = build_paragraph_occurrences(cob_lines_with_linenos)
    dups: List[Tuple[int, str]] = []
    for name, lines in occ.items():
        if len(lines) > 1:
            for lnno in lines[1:]:
                dups.append((lnno, name))
    return sorted(dups)


def find_duplicate_sections(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str]]:
    """Return list of (line_no, section_name) for duplicate section declarations (excluding first occurrence)."""
    occ: Dict[str, List[int]] = {}
    for name, lnno in get_section_decls_from_source(cob_lines_with_linenos):
        occ.setdefault(name, []).append(lnno)

    dups: List[Tuple[int, str]] = []
    for name, lines in occ.items():
        if len(lines) > 1:
            for lnno in lines[1:]:
                dups.append((lnno, name))
    return sorted(dups)


def find_paragraph_prefix_mismatches(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str, str]]:
    """Return list of (line_no, paragraph_name, section_name) when paragraph prefix != section prefix."""
    section_decls = build_section_map(cob_lines_with_linenos)
    mism: List[Tuple[int, str, str]] = []
    for lnno, pname in get_paragraph_decls(cob_lines_with_linenos):
        sec = current_section_at_line(section_decls, lnno)
        if not sec:
            continue
        if "-" not in pname and "_" not in pname:
            continue

        pfx = section_prefix(pname)
        sfx = section_prefix(sec)
        if pfx and sfx and pfx != sfx:
            mism.append((lnno, pname, sec))

    return sorted(mism)


def find_evaluate_without_when_other(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[int]:
    """Return list of EVALUATE start line numbers that do not contain WHEN OTHER before END-EVALUATE."""
    eval_rx = re.compile(r"\bEVALUATE\b", re.IGNORECASE)
    when_other_rx = re.compile(r"\bWHEN\s+OTHER\b", re.IGNORECASE)
    end_eval_rx = re.compile(r"\bEND-EVALUATE\b", re.IGNORECASE)

    stack: List[Dict[str, object]] = []
    viols: List[int] = []
    components = parse_cobol_components(cob_lines_with_linenos)

    def _is_standalone_keyword(line_txt: str, match: re.Match) -> bool:
        start, end = match.start(0), match.end(0)
        prev = line_txt[start - 1] if start > 0 else " "
        nxt = line_txt[end] if end < len(line_txt) else " "
        if prev.isalnum() or prev in "-_":
            return False
        if nxt.isalnum() or nxt in "-_":
            return False
        return True

    for line in components.lines:
        t = _strip_string_literals(line.clean)
        if not t.strip():
            continue
        if not is_executable_line(line):
            continue

        # Start of EVALUATE (not END-EVALUATE)
        eval_m = eval_rx.search(t)
        end_eval_m = end_eval_rx.search(t)
        if eval_m and _is_standalone_keyword(t, eval_m) and not (end_eval_m and _is_standalone_keyword(t, end_eval_m)):
            stack.append({"start_ln": line.line_no, "has_when_other": False})

        if stack and when_other_rx.search(t):
            stack[-1]["has_when_other"] = True

        if end_eval_m and _is_standalone_keyword(t, end_eval_m) and stack:
            frame = stack.pop()
            if not frame.get("has_when_other", False):
                viols.append(int(frame.get("start_ln", line.line_no)))

    # Unclosed evaluates -> still report
    while stack:
        frame = stack.pop()
        if not frame.get("has_when_other", False):
            viols.append(int(frame.get("start_ln", 0)))

    return sorted(set([v for v in viols if v]))


# ---------------------------
# Divisions/helpers
# ---------------------------

def find_divisions_in_source(cob_lines_with_linenos: List[Tuple[int, str]]) -> Dict[str, int]:
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


def filter_to_procedure_division(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str]]:
    """
    Return lines at or after the PROCEDURE DIVISION declaration.
    If PROCEDURE DIVISION is not found, returns the original list.
    """
    proc_ln = find_procedure_division_line(cob_lines_with_linenos)
    if not proc_ln:
        return cob_lines_with_linenos
    return [(lnno, txt) for lnno, txt in cob_lines_with_linenos if lnno >= proc_ln]
