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
    rtype: str = "REGEX"  # REGEX | MAX_SECTIONS | REQUIRED_SECTIONS | REQUIRED_DIVISIONS | UNUSED_SECTIONS | GOTO_EXIT_ONLY_SAME_SECTION |
                          # PROGRAM_ID_MATCH | UNIQUE_PARAGRAPHS | PARAGRAPH_PREFIX_MATCH | EVALUATE_WHEN_OTHER
    max_value: int = 0
    required_list: List[str] = None
    exclude_list: List[str] = None

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

ACRT_VERSION = "0.2.0"

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


def acrt_tag() -> str:
    return f"ACRT v{ACRT_VERSION}"


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
        line = re.sub(r"\*>.*$", "", line)  # strip inline comment
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
        exclude_list = parse_required_list(r.get("exclude") or "")

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

        # Rule types that do not require <Code> or required_list; exclude_list is optional
        if rtype in (
            "UNUSED_SECTIONS",
            "GOTO_EXIT_ONLY_SAME_SECTION",
            "PROGRAM_ID_MATCH",
            "UNIQUE_PARAGRAPHS",
            "PARAGRAPH_PREFIX_MATCH",
            "EVALUATE_WHEN_OTHER",
        ):
            pass

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
                exclude_list=exclude_list,
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
    max_samples_per_rule: int = 5
) -> Tuple[Dict[str, int], List[RuleMatch]]:
    """
    Apply rules to the given cleaned lines in a given context (MASTER/LOCAL/DIFF).
    For source-level rules we use the COBOL source index for accuracy and line numbers.
    """
    counts = {"E": 0, "W": 0, "I": 0}
    matches: List[RuleMatch] = []

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
                matches.append(RuleMatch(rule=rule, count=1, sample_lines=samples, src_locations=[1]))
            elif expected_norm != found_norm:
                counts[rule.severity] += 1
                samples = [f"PROGRAM-ID={prog_id} (expected {expected_prog_base})"]
                src_locs = [prog_ln] if prog_ln else [1]
                matches.append(RuleMatch(rule=rule, count=1, sample_lines=samples, src_locations=src_locs))
            continue

        # --------------------
        # UNIQUE_PARAGRAPHS (3.22)
        # --------------------
        if rule.rtype == "UNIQUE_PARAGRAPHS":
            if not cob_lines_with_linenos:
                continue

            dups = find_duplicate_paragraphs(cob_lines_with_linenos)
            if dups:
                counts[rule.severity] += len(dups)
                samples: List[str] = [f"Duplicate PARAGRAPH(s) count={len(dups)}"]
                for lnno, name in dups[:max_samples_per_rule]:
                    samples.append(f"Duplicate PARAGRAPH: {name} at line {lnno}")
                src_locs = [lnno for lnno, _ in dups]
                matches.append(RuleMatch(rule=rule, count=len(dups), sample_lines=samples, src_locations=src_locs))
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
                matches.append(RuleMatch(rule=rule, count=len(mism), sample_lines=samples, src_locations=src_locs))
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
                matches.append(RuleMatch(rule=rule, count=len(viols), sample_lines=samples, src_locations=viols))
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

                matches.append(RuleMatch(rule=rule, count=1, sample_lines=samples, src_locations=src_locs))
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
                matches.append(RuleMatch(rule=rule, count=1, sample_lines=samples, src_locations=src_locs))
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
                matches.append(RuleMatch(rule=rule, count=1, sample_lines=samples, src_locations=src_locs))
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
                matches.append(RuleMatch(rule=rule, count=len(unused), sample_lines=samples, src_locations=src_locs))
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
                matches.append(RuleMatch(rule=rule, count=len(viols), sample_lines=samples, src_locations=src_locs))
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


# ---------------------------
# Main
# ---------------------------

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

    # Diff lines
    diff_lines = compute_local_only_diff(master_clean, private_clean)

    # Apply rules per context
    master_counts, master_matches = apply_rules(
        master_clean, rules, context="MASTER", expected_prog_base=expected_prog_base, cob_lines_with_linenos=cob_lines_with_linenos
    )
    local_counts, local_matches = apply_rules(
        private_clean, rules, context="LOCAL", expected_prog_base=expected_prog_base, cob_lines_with_linenos=cob_lines_with_linenos
    )
    diff_counts, diff_matches = apply_rules(
        diff_lines, rules, context="DIFF", expected_prog_base=expected_prog_base, cob_lines_with_linenos=cob_lines_with_linenos
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

    fail = (
        abs_err > thresholds.get("errors", 0)
        or abs_wrn > thresholds.get("warnings", 0)
        or abs_inf > thresholds.get("infos", 0)
    )

    # Full report (goes to STATUS FILE)
    report_lines: List[str] = []
    report_lines.append(f"Getting current {acrt_tag()} status for {base} on {now_str()} (user: {current_user()}):\n")
    report_lines.append(f"On Master build directory : #Errors {master_counts['E']} #Warnings {master_counts['W']} #Infos {master_counts['I']}\n")
    report_lines.append(f"On Private build directory : #Errors {local_counts['E']} #Warnings {local_counts['W']} #Infos {local_counts['I']}\n")
    report_lines.append(
        "The difference : "
        f"Errors {diff_epilog['E']} "
        f"Warnings {diff_epilog['W']} "
        f"Infos {diff_epilog['I']}\n"
    )
    report_lines.append("\n")

    report_lines.append(format_rule_matches(
        "Rule matches on LOCAL-ONLY DIFF (Private - Master):",
        diff_matches,
        base
    ))
    report_lines.append("\n")

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
