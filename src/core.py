import bisect
import datetime
import getpass
import os
import re
import sys
from typing import Dict, List, Tuple

import fcntl


ACCEPTED_EXTS = (".cob", ".inc", ".pco")

ACRT_VERSION = "0.3.2"


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


def _normalize_line_for_map(line: str) -> str:
    t = line.strip().upper()
    t = re.sub(r"^\d+\s+", "", t)
    t = re.sub(r"\s+", "", t)
    return t


def _normalize_line_for_map_loose(line: str) -> str:
    t = line.strip().upper()
    t = re.sub(r"^\d+\s+", "", t)
    t = re.sub(r"[^A-Z0-9]", "", t)
    return t


def _build_norm_index(norm_entries: List[Tuple[str, int]]) -> Dict[str, Tuple[List[int], List[int]]]:
    index: Dict[str, Tuple[List[int], List[int]]] = {}
    for pos, (norm, lnno) in enumerate(norm_entries):
        if not norm:
            continue
        if norm not in index:
            index[norm] = ([], [])
        index[norm][0].append(pos)
        index[norm][1].append(lnno)
    return index


def _find_next_in_index(index: Dict[str, Tuple[List[int], List[int]]], key: str, start_pos: int) -> Tuple[int, int]:
    if not key:
        return 0, start_pos
    bucket = index.get(key)
    if not bucket:
        return 0, start_pos
    positions, linenos = bucket
    i = bisect.bisect_left(positions, start_pos)
    if i >= len(positions):
        return 0, start_pos
    return linenos[i], positions[i] + 1


def _find_window_fuzzy(
    norm_entries: List[Tuple[str, int]],
    key: str,
    start_pos: int,
    window: int = 2000,
) -> Tuple[int, int]:
    if not key:
        return 0, start_pos
    lo = max(0, start_pos - window)
    hi = min(len(norm_entries), start_pos + window)
    best_pos = -1
    best_ln = 0
    for pos in range(lo, hi):
        cand, lnno = norm_entries[pos]
        if not cand:
            continue
        if key == cand:
            return lnno, pos + 1
        if key in cand or cand in key:
            best_pos = pos
            best_ln = lnno
            break
    if best_pos >= 0:
        return best_ln, best_pos + 1
    return 0, start_pos


def map_listing_lines_to_cob_lines(
    listing_lines: List[str],
    cob_lines_with_linenos: List[Tuple[int, str]],
) -> List[int]:
    """
    Map listing lines to COBOL source line numbers using sequential matching
    with strict/loose normalization fallback.
    Unmatched lines map to 0.
    """
    cob_norm_strict = [(_normalize_line_for_map(txt), lnno) for lnno, txt in cob_lines_with_linenos]
    cob_norm_loose = [(_normalize_line_for_map_loose(txt), lnno) for lnno, txt in cob_lines_with_linenos]
    strict_index = _build_norm_index(cob_norm_strict)
    loose_index = _build_norm_index(cob_norm_loose)
    mapping: List[int] = []
    cob_idx = 0

    for line in listing_lines:
        norm = _normalize_line_for_map(line)
        if not norm:
            mapping.append(0)
            continue

        found, next_idx = _find_next_in_index(strict_index, norm, cob_idx)
        if not found:
            loose = _normalize_line_for_map_loose(line)
            found, next_idx = _find_next_in_index(loose_index, loose, cob_idx)
        if not found:
            loose = _normalize_line_for_map_loose(line)
            found, next_idx = _find_window_fuzzy(cob_norm_loose, loose, cob_idx)
        if found:
            cob_idx = next_idx
        mapping.append(found)

    return mapping


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

    alm = os.path.expandvars(alm)
    loc = os.path.expandvars(loc)

    alm_path = os.path.join(alm, "target", "obj", lis_name)
    loc_path = os.path.join(loc, "target", "obj", lis_name)

    if not os.path.isfile(alm_path):
        error_exit(f"ALM listing file not found: {alm_path}")
    if not os.path.isfile(loc_path):
        error_exit(f"Local listing file not found: {loc_path}")

    return alm_path, loc_path
