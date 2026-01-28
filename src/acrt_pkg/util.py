import fnmatch
import os
import re
import tempfile
from datetime import datetime


def now_timestamp():
    return datetime.now().strftime("%Y-%m-%d %H:%M:%S")


def normalize_whitespace(text):
    text = text.replace("\t", " ")
    text = re.sub(r"\s+", " ", text.strip())
    return text


def is_comment_line(line):
    stripped = line.lstrip()
    return stripped.startswith("*") if stripped else False


def matches_any_pattern(name, patterns):
    for pat in patterns:
        if fnmatch.fnmatch(name, pat):
            return True
    return False


def atomic_write(path, content):
    directory = os.path.dirname(path)
    fd, tmp_path = tempfile.mkstemp(prefix=".acrt_", dir=directory)
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as f:
            f.write(content)
        os.replace(tmp_path, path)
    finally:
        try:
            if os.path.exists(tmp_path):
                os.unlink(tmp_path)
        except OSError:
            pass


def extract_program_id(line):
    if "PROGRAM-ID" not in line:
        return None
    normalized = line.replace("PROGRAM-ID.", "PROGRAM-ID")
    parts = normalized.split()
    if not parts or parts[0] != "PROGRAM-ID":
        return None
    if len(parts) < 2:
        return None
    return parts[1].rstrip(".")
