from dataclasses import dataclass
import os
from difflib import SequenceMatcher
from bisect import bisect_left
from .util import normalize_whitespace, is_comment_line


@dataclass
class CleanedListing:
    lines: list
    line_map: list
    col1: list
    path: str


def read_listing(path):
    encoding = os.getenv("ACRT_LANG", "iso-8859-8")
    try:
        f = open(path, "r", encoding=encoding, errors="replace")
    except LookupError:
        f = open(path, "r", encoding="utf-8", errors="replace")
    with f:
        raw_lines = f.readlines()
    return raw_lines


def _clean_lines_with_map(raw_lines):
    cleaned = []
    for idx, line in enumerate(raw_lines, start=1):
        line = line.rstrip("\n")
        if is_comment_line(line):
            continue
        col1 = line[:1] != " "
        normalized = normalize_whitespace(line)
        if not normalized:
            continue
        normalized = normalized.upper()
        cleaned.append((normalized, idx, col1))
    return cleaned


def clean_listing(raw_lines, path, source_cleaned=None):
    cleaned = []
    line_map = []
    col1_flags = []
    cleaned_listing = _clean_lines_with_map(raw_lines)
    if source_cleaned:
        source_texts = [t for t, _ln, _c1 in source_cleaned]
        listing_texts = [t for t, _ln, _c1 in cleaned_listing]
        matcher = SequenceMatcher(None, source_texts, listing_texts, autojunk=False)
        listing_to_source = {}
        for i1, j1, size in matcher.get_matching_blocks():
            for offset in range(size):
                listing_to_source[j1 + offset] = source_cleaned[i1 + offset][1]
        source_positions = {}
        for i, (text, _line_no, _c1) in enumerate(source_cleaned):
            source_positions.setdefault(text, []).append(i)
        last_source_idx = 0
        for idx, (normalized, _lis_line, col1) in enumerate(cleaned_listing):
            source_line = listing_to_source.get(idx)
            if source_line is None:
                positions = source_positions.get(normalized)
                if positions:
                    pos_idx = bisect_left(positions, last_source_idx)
                    if pos_idx < len(positions):
                        src_idx = positions[pos_idx]
                        source_line = source_cleaned[src_idx][1]
                        last_source_idx = src_idx + 1
            cleaned.append(normalized)
            line_map.append(source_line)
            col1_flags.append(col1)
    else:
        for normalized, idx, col1 in cleaned_listing:
            cleaned.append(normalized)
            line_map.append(idx)
            col1_flags.append(col1)
    return CleanedListing(lines=cleaned, line_map=line_map, col1=col1_flags, path=path)


def clean_source(raw_lines):
    return _clean_lines_with_map(raw_lines)
