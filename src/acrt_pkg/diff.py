from collections import Counter
from .listing import CleanedListing


def local_only_diff(master: CleanedListing, local: CleanedListing):
    master_counts = Counter(master.lines)
    diff_lines = []
    diff_map = []
    diff_col1 = []
    for line, line_no, col1 in zip(local.lines, local.line_map, local.col1):
        if master_counts.get(line, 0) > 0:
            master_counts[line] -= 1
            continue
        diff_lines.append(line)
        diff_map.append(line_no)
        diff_col1.append(col1)
    return CleanedListing(lines=diff_lines, line_map=diff_map, col1=diff_col1, path="<diff>")
