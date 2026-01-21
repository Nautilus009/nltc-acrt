import re
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


# ---------------------------
# GO TO rule helpers
# ---------------------------

def build_section_map(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str]]:
    """Return a list of (line_number, SECTION-NAME) for each SECTION declaration (excluding WORKING-STORAGE/LINKAGE)."""
    sec_rx = re.compile(r"^\s*([A-Z0-9][A-Z0-9-]*)\s+SECTION\s*\.", re.IGNORECASE)
    out: List[Tuple[int, str]] = []
    for lnno, txt in cob_lines_with_linenos:
        m = sec_rx.search(txt)
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

    # Paragraph label form:
    #   X-ABC.
    #   X-ABC. <statement>
    par_rx = re.compile(r"^\s*([A-Z0-9][A-Z0-9-]*)\s*\.(?:\s+.*)?$", re.IGNORECASE)

    idx: Dict[str, Tuple[str, int]] = {}
    for lnno, txt in cob_lines_with_linenos:
        t = re.sub(r"\*>.*$", "", txt).rstrip()
        if not t:
            continue
        if re.search(r"\bSECTION\b", t, re.IGNORECASE) or re.search(r"\bDIVISION\b", t, re.IGNORECASE):
            continue

        m = par_rx.match(t)
        if not m:
            continue

        name = (m.group(1) or "").upper()
        if is_reserved_paragraph_label(name):
            continue

        sec = current_section_at_line(section_decls, lnno)
        if name not in idx:
            idx[name] = (sec, lnno)

    return idx


def _is_paragraph_label_line(txt: str) -> bool:
    """Heuristic: paragraph label line looks like 'X-YYY.' and is not a DIVISION/SECTION line."""
    if re.search(r"\bSECTION\b", txt, re.IGNORECASE):
        return False
    if re.search(r"\bDIVISION\b", txt, re.IGNORECASE):
        return False
    m = re.match(r"^\s*([A-Z0-9][A-Z0-9-]*)\s*\.(?:\s+.*)?$", txt, re.IGNORECASE)
    if not m:
        return False
    name = (m.group(1) or "").upper()
    if is_reserved_paragraph_label(name):
        return False
    return True


def get_paragraph_decls(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str]]:
    """Return paragraph declarations as (line_number, paragraph_name)."""
    par_rx = re.compile(r"^\s*([A-Z0-9][A-Z0-9-]*)\s*\.(?:\s+.*)?$", re.IGNORECASE)
    decls: List[Tuple[int, str]] = []
    for lnno, txt in cob_lines_with_linenos:
        t = re.sub(r"\*>.*$", "", txt).rstrip()
        if not t:
            continue
        if re.search(r"\bSECTION\b", t, re.IGNORECASE) or re.search(r"\bDIVISION\b", t, re.IGNORECASE):
            continue
        m = par_rx.match(t)
        if not m:
            continue
        name = (m.group(1) or "").upper()
        if is_reserved_paragraph_label(name):
            continue
        decls.append((lnno, name))
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

    in_para = False
    decl_ln = 0
    body: List[str] = []

    for lnno, txt in cob_lines_with_linenos:
        t = re.sub(r"\*>.*$", "", txt).rstrip()

        if not in_para:
            m = label_rx.match(t)
            if not m:
                continue
            in_para = True
            decl_ln = lnno
            rest = (m.group(1) or "").strip()
            if rest:
                body.append(rest)
            continue

        # Stop at next paragraph label or any section/division line
        if _is_paragraph_label_line(t) or re.search(r"\bSECTION\b", t, re.IGNORECASE) or re.search(r"\bDIVISION\b", t, re.IGNORECASE):
            break

        if not t:
            continue
        if t.startswith("*"):
            continue
        if t.strip() == "":
            continue
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
        r"\bGO\s+TO\s+([A-Z0-9][A-Z0-9-]*)\b|\bGOTO\s+([A-Z0-9][A-Z0-9-]*)\b",
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
    perform_target_rx = re.compile(r"\bPERFORM\s+([A-Z0-9][A-Z0-9-]*)\b", re.IGNORECASE)

    stack: List[Tuple[str, int]] = []
    lines = cob_lines_with_linenos

    def _next_significant_line(start_idx: int) -> str:
        for _i in range(start_idx + 1, len(lines)):
            _txt = re.sub(r"\*>.*$", "", lines[_i][1]).strip()
            if not _txt:
                continue
            if _txt.startswith("*"):
                continue
            return _txt
        return ""

    next_target_rx = re.compile(r"^\s*([A-Z0-9][A-Z0-9-]*)\b", re.IGNORECASE)

    def _is_standalone_token(line: str, match: re.Match) -> bool:
        start, end = match.start(1), match.end(1)
        prev = line[start - 1] if start > 0 else " "
        nxt = line[end] if end < len(line) else " "
        if prev.isalnum() or prev == "-":
            return False
        if nxt.isalnum() or nxt == "-":
            return False
        return True

    for _idx, (lnno, txt) in enumerate(lines):
        t = re.sub(r"\*>.*$", "", txt)
        if not t.strip():
            continue
        if re.search(r"\bSECTION\b", t, re.IGNORECASE) or re.search(r"\bDIVISION\b", t, re.IGNORECASE):
            continue
        if _is_paragraph_label_line(t):
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
                    stack.append(("PERFORM", lnno))
                elif not tgt:
                    stack.append(("PERFORM", lnno))
                continue

            stack.append((token, lnno))

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
    token_rx = re.compile(r"^\s*([A-Z0-9-]+)\b", re.IGNORECASE)

    violations: List[Tuple[int, str]] = []
    for lnno, txt in cob_lines_with_linenos:
        t = re.sub(r"\*>.*$", "", txt).rstrip()
        if not t.strip():
            continue
        if re.search(r"\bSECTION\b", t, re.IGNORECASE) or re.search(r"\bDIVISION\b", t, re.IGNORECASE):
            continue
        if _is_paragraph_label_line(t):
            continue

        cur_sec = current_section_at_line(section_decls, lnno)
        if cur_sec != "A-MAIN":
            continue

        m = token_rx.search(t)
        if not m:
            continue
        token = (m.group(1) or "").upper()
        if token in allowed_tokens:
            continue
        if token in COBOL_RESERVED_LABELS:
            violations.append((lnno, t.strip()))

    return violations


def find_recursive_perform_calls(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str, str]]:
    """Return list of (line_no, src_paragraph, target_paragraph) where a PERFORM is recursive."""
    par_decls = get_paragraph_decls(cob_lines_with_linenos)
    par_idx = {name: lnno for lnno, name in par_decls}
    perform_rx = re.compile(r"\bPERFORM\s+([A-Z0-9][A-Z0-9-]*)\b", re.IGNORECASE)

    edges: List[Tuple[str, str, int]] = []
    for lnno, txt in cob_lines_with_linenos:
        t = re.sub(r"\*>.*$", "", txt)
        if _is_paragraph_label_line(t):
            continue
        src = current_paragraph_at_line(par_decls, lnno)
        if not src:
            continue
        for m in perform_rx.finditer(t):
            target = (m.group(1) or "").upper()
            if target in par_idx:
                edges.append((src, target, lnno))

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

    call_lit_rx = re.compile(r"\bCALL\s+['\"]([A-Z0-9][A-Z0-9-]*)['\"]", re.IGNORECASE)
    viols: List[Tuple[int, str]] = []

    for lnno, txt in cob_lines_with_linenos:
        t = re.sub(r"\*>.*$", "", txt)
        for m in call_lit_rx.finditer(t):
            target = (m.group(1) or "").upper()
            if normalize_name_for_compare(target) == normalize_name_for_compare(prog_id):
                viols.append((lnno, target))

    return viols


def _normalize_code_lines(lines: List[str]) -> List[str]:
    norm: List[str] = []
    for line in lines:
        t = re.sub(r"\*>.*$", "", line)
        t = re.sub(r"\"[^\"]*\"|'[^']*'", "S", t)
        t = re.sub(r"\d+", "N", t)
        t = re.sub(r"\s+", "", t)
        if t:
            norm.append(t)
    return norm


def find_duplicate_code_blocks(cob_lines_with_linenos: List[Tuple[int, str]], min_lines: int = 2) -> List[Tuple[int, str, str]]:
    """
    Return list of (line_no, paragraph_name, original_paragraph) for duplicate code blocks.
    Uses normalized paragraph bodies as fingerprints.
    """
    decls = get_paragraph_decls(cob_lines_with_linenos)
    seen: Dict[str, Tuple[str, int]] = {}
    dups: List[Tuple[int, str, str]] = []

    for lnno, pname in decls:
        _decl_ln, body = get_paragraph_body_lines(cob_lines_with_linenos, pname)
        norm = _normalize_code_lines(body)
        if len(norm) < min_lines:
            continue
        fingerprint = "\n".join(norm)
        if fingerprint in seen:
            orig_name, _orig_ln = seen[fingerprint]
            dups.append((lnno, pname, orig_name))
        else:
            seen[fingerprint] = (pname, lnno)

    return dups


def find_long_numeric_literals(cob_lines_with_linenos: List[Tuple[int, str]], min_len: int = 8) -> List[Tuple[int, str]]:
    """Return list of (line_no, literal_snippet) for string literals containing long digit sequences."""
    lit_rx = re.compile(r"(['\"])(.*?)\1")
    digits_rx = re.compile(rf"\d{{{min_len},}}")
    viols: List[Tuple[int, str]] = []

    for lnno, txt in cob_lines_with_linenos:
        t = re.sub(r"\*>.*$", "", txt)
        for m in lit_rx.finditer(t):
            lit = m.group(2) or ""
            if digits_rx.search(lit):
                viols.append((lnno, lit))

    return viols


def build_paragraph_occurrences(cob_lines_with_linenos: List[Tuple[int, str]]) -> Dict[str, List[int]]:
    """Return paragraph name -> list of declaration line numbers (all occurrences)."""
    section_decls = build_section_map(cob_lines_with_linenos)
    par_rx = re.compile(r"^\s*([A-Z0-9][A-Z0-9-]*)\s*\.(?:\s+.*)?$", re.IGNORECASE)

    occ: Dict[str, List[int]] = {}
    for lnno, txt in cob_lines_with_linenos:
        t = re.sub(r"\*>.*$", "", txt).rstrip()
        if not t:
            continue
        if re.search(r"\bSECTION\b", t, re.IGNORECASE) or re.search(r"\bDIVISION\b", t, re.IGNORECASE):
            continue

        m = par_rx.match(t)
        if not m:
            continue

        name = (m.group(1) or "").upper()
        if is_reserved_paragraph_label(name):
            continue

        # Reduce false positives: in our conventions paragraph labels include '-'
        if "-" not in name:
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


def find_paragraph_prefix_mismatches(cob_lines_with_linenos: List[Tuple[int, str]]) -> List[Tuple[int, str, str]]:
    """Return list of (line_no, paragraph_name, section_name) when paragraph prefix != section prefix."""
    section_decls = build_section_map(cob_lines_with_linenos)
    par_rx = re.compile(r"^\s*([A-Z0-9][A-Z0-9-]*)\s*\.(?:\s+.*)?$", re.IGNORECASE)

    mism: List[Tuple[int, str, str]] = []
    for lnno, txt in cob_lines_with_linenos:
        t = re.sub(r"\*>.*$", "", txt).rstrip()
        if not t:
            continue
        if re.search(r"\bSECTION\b", t, re.IGNORECASE) or re.search(r"\bDIVISION\b", t, re.IGNORECASE):
            continue

        m = par_rx.match(t)
        if not m:
            continue

        pname = (m.group(1) or "").upper()
        if is_reserved_paragraph_label(pname):
            continue

        sec = current_section_at_line(section_decls, lnno)
        if not sec:
            continue

        if "-" not in pname:
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

    for lnno, txt in cob_lines_with_linenos:
        t = re.sub(r"\*>.*$", "", txt)
        if not t.strip():
            continue

        # Start of EVALUATE (not END-EVALUATE)
        if eval_rx.search(t) and not end_eval_rx.search(t):
            stack.append({"start_ln": lnno, "has_when_other": False})

        if stack and when_other_rx.search(t):
            stack[-1]["has_when_other"] = True

        if end_eval_rx.search(t) and stack:
            frame = stack.pop()
            if not frame.get("has_when_other", False):
                viols.append(int(frame.get("start_ln", lnno)))

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
