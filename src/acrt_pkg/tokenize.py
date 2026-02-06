import re
from dataclasses import dataclass, field
from typing import Optional
from .util import extract_program_id

DIVISION_RE = re.compile(r"^\s*(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION(?:\s+.*)?\.?\s*$")
SECTION_RE = re.compile(r"^\s*([A-Z0-9_-]+)\s+SECTION\.\s*$")
SECTION_LIKE_RE = re.compile(r"^\s*(FILE-CONTROL|SPECIAL-NAMES|I-O-CONTROL)\.\s*$")
PARA_ONLY_RE = re.compile(r"^([A-Z0-9][A-Z0-9_-]*)\.\s*$")
PARA_INLINE_RE = re.compile(r"^([A-Z0-9][A-Z0-9_-]*)\.\s+(.+)$")
END_LABEL_RE = re.compile(r"^\s*END-[A-Z0-9_-]+\.\s*$")
IDENT_TOKEN_RE = re.compile(r"\b[A-Z][A-Z0-9_-]*\b")
DATA_ITEM_RE = re.compile(r"^(0[1-9]|[1-7][0-9]|77|88)\s+([A-Z][A-Z0-9_-]*)")
VALUE_RE = re.compile(r"\bVALUE(?:\s+IS)?\s+([^\.]+)")
VALUE_ONLY_RE = re.compile(r"\bVALUE(?:\s+IS)?\s*$")
PIC_RE = re.compile(r"\bPIC(?:TURE)?\s+([A-Z0-9(),V\.\-\+]+)")

BLOCK_STARTERS = {
    "IF",
    "EVALUATE",
    "READ",
    "WRITE",
    "REWRITE",
    "DELETE",
    "START",
    "SEARCH",
    "STRING",
    "UNSTRING",
    "EXEC",
}

COMMAND_KEYWORDS = {
    "ACCEPT",
    "ADD",
    "CALL",
    "CANCEL",
    "CLOSE",
    "COMPUTE",
    "CONTINUE",
    "COPY",
    "DELETE",
    "DISPLAY",
    "DIVIDE",
    "EVALUATE",
    "EXEC",
    "EXIT",
    "GO",
    "GOBACK",
    "IF",
    "INITIALIZE",
    "MOVE",
    "MULTIPLY",
    "OPEN",
    "PERFORM",
    "READ",
    "REWRITE",
    "RETURN",
    "SEARCH",
    "SET",
    "START",
    "STOP",
    "STRING",
    "SUBTRACT",
    "UNSTRING",
    "WRITE",
}

EXPLICIT_ENDERS = {
    "END-IF": "IF",
    "END-EVALUATE": "EVALUATE",
    "END-PERFORM": "PERFORM",
    "END-READ": "READ",
    "END-WRITE": "WRITE",
    "END-REWRITE": "REWRITE",
    "END-DELETE": "DELETE",
    "END-START": "START",
    "END-SEARCH": "SEARCH",
    "END-STRING": "STRING",
    "END-UNSTRING": "UNSTRING",
    "END-EXEC": "EXEC",
}

BRANCH_MARKERS = {
    "ELSE",
    "THEN",
    "WHEN",
    "WHEN OTHER",
    "AT END",
    "NOT AT END",
    "INVALID KEY",
    "NOT INVALID KEY",
    "ON EXCEPTION",
    "NOT ON EXCEPTION",
}

INLINE_PERFORM_RE = re.compile(
    r"\bPERFORM\b.*\b(UNTIL|VARYING|TIMES|FOREVER|WITH\s+TEST|TEST\s+BEFORE|TEST\s+AFTER)\b"
)


@dataclass
class Node:
    kind: str
    name: str
    start_line: int
    end_line: int
    text_lines: list = field(default_factory=list)
    parent: Optional["Node"] = None
    children: list = field(default_factory=list)
    division: Optional[str] = None
    section: Optional[str] = None
    paragraph: Optional[str] = None
    closed_by: Optional[str] = None
    keywords: set = field(default_factory=set)
    normalized_text: str = ""
    meta: dict = field(default_factory=dict)

    def add_child(self, node):
        self.children.append(node)
        node.parent = self

    def path(self):
        parts = []
        if self.division:
            parts.append(self.division)
        if self.section:
            parts.append(self.section)
        if self.paragraph:
            parts.append(self.paragraph)
        if self.kind == "COMMAND" and self.name:
            parts.append(self.name)
        return "/".join(parts)


@dataclass
class TokenizationContext:
    tree: Node
    by_kind: dict
    stats: dict
    anomalies: list
    meta: dict = field(default_factory=dict)


_STRING_RE = re.compile(r"\"[^\"]*\"|'[^']*'")


def _strip_strings(line):
    return _STRING_RE.sub(" ", line)


def _keyword_set(line):
    return set(IDENT_TOKEN_RE.findall(_strip_strings(line)))


def _first_keyword(line):
    m = IDENT_TOKEN_RE.search(_strip_strings(line))
    return m.group(0) if m else ""


def _detect_ender(line):
    for ender, starter in EXPLICIT_ENDERS.items():
        if re.search(rf"\b{re.escape(ender)}\b", line):
            return ender, starter
    return None, None


def _detect_branch(line):
    for marker in sorted(BRANCH_MARKERS, key=len, reverse=True):
        if re.search(rf"\b{re.escape(marker)}\b", line):
            return marker
    return None


def _is_inline_perform(line):
    return INLINE_PERFORM_RE.search(line) is not None


def tokenize(cleaned_listing, element_name):
    root = Node(kind="PROGRAM", name=element_name, start_line=1, end_line=1)
    by_kind = {"PROGRAM": [root], "DIVISION": [], "SECTION": [], "PARAGRAPH": [], "COMMAND": [], "VARIABLE": []}
    anomalies = []

    current_div = None
    current_sec = None
    current_para = None
    current_command = None
    command_stack = []
    in_procedure = False
    last_real_line_no = 1
    prev_line_dot = False
    prev_line_was_section = False
    pending_value_var = None

    def safe_line_no(line_no):
        return line_no if line_no is not None else last_real_line_no

    def prev_line_no(line_no):
        return safe_line_no(line_no) - 1

    def close_open_commands(closure, line_no):
        while command_stack:
            node = command_stack.pop()
            node.end_line = safe_line_no(line_no)
            node.closed_by = closure

    def append_to_command(node, line, line_no):
        node.text_lines.append(line)
        node.normalized_text = " ".join(node.text_lines)
        node.keywords |= _keyword_set(line)
        node.end_line = safe_line_no(line_no)

    program_id = None
    for line, line_no, col1 in zip(cleaned_listing.lines, cleaned_listing.line_map, cleaned_listing.col1):
        if line_no is not None:
            last_real_line_no = line_no
        if program_id is None:
            program_id = extract_program_id(line)
            if program_id:
                root.name = program_id
        if pending_value_var and current_div and current_div.name == "DATA":
            literal = line.strip()
            if literal.endswith("."):
                literal = literal[:-1].rstrip()
            if (literal.startswith('"') and literal.endswith('"')) or (literal.startswith("'") and literal.endswith("'")):
                pending_value_var.meta["value"] = literal
                pending_value_var.meta["value_explicit"] = True
                pending_value_var = None
                prev_line_dot = line.strip().endswith(".")
                prev_line_was_section = False
                continue
        div_match = DIVISION_RE.match(line)
        if div_match:
            close_open_commands("DIVISION", line_no)
            if current_sec:
                current_sec.end_line = prev_line_no(line_no)
                current_sec = None
            if current_para:
                current_para.end_line = prev_line_no(line_no)
                current_para = None
            current_command = None
            prev_line_dot = False
            prev_line_was_section = False
            div_name = div_match.group(1)
            current_div = Node(kind="DIVISION", name=div_name, start_line=safe_line_no(line_no), end_line=safe_line_no(line_no))
            current_div.division = div_name
            root.add_child(current_div)
            by_kind["DIVISION"].append(current_div)
            current_sec = None
            current_para = None
            in_procedure = div_name == "PROCEDURE"
            continue

        sec_match = SECTION_RE.match(line)
        sec_like_match = None
        if not sec_match:
            sec_like_match = SECTION_LIKE_RE.match(line)
        if sec_match or sec_like_match:
            sec_name = sec_match.group(1) if sec_match else sec_like_match.group(1)
            if current_sec:
                current_sec.end_line = prev_line_no(line_no)
            current_sec = Node(kind="SECTION", name=sec_name, start_line=safe_line_no(line_no), end_line=safe_line_no(line_no))
            current_sec.division = current_div.name if current_div else None
            current_sec.section = sec_name
            if current_div:
                current_div.add_child(current_sec)
            else:
                root.add_child(current_sec)
            by_kind["SECTION"].append(current_sec)
            if current_para:
                current_para.end_line = prev_line_no(line_no)
                current_para = None
            current_para = None
            current_command = None
            prev_line_dot = line.strip().endswith(".")
            prev_line_was_section = True
            continue

        keyword = _first_keyword(line)
        ender, _starter = _detect_ender(line)
        branch = _detect_branch(line)
        is_structural = False
        include_node = False
        line_ends = line.strip().endswith(".")

        if current_div and current_div.name == "DATA":
            data_match = DATA_ITEM_RE.match(line)
            if data_match:
                level = data_match.group(1)
                var_name = data_match.group(2)
                if var_name != "FILLER":
                    var_line = line_no if line_no is not None else None
                    var_node = Node(kind="VARIABLE", name=var_name, start_line=var_line, end_line=var_line)
                    var_node.division = current_div.name
                    var_node.section = current_sec.name if current_sec else None
                    var_node.paragraph = current_para.name if current_para else None
                    value, explicit = _infer_var_value(line)
                    var_node.meta["level"] = level
                    var_node.meta["value"] = value
                    var_node.meta["value_explicit"] = explicit
                    if not explicit and VALUE_ONLY_RE.search(line):
                        pending_value_var = var_node
                    parent = current_para or current_sec or current_div
                    if parent:
                        parent.add_child(var_node)
                    by_kind["VARIABLE"].append(var_node)
                prev_line_dot = line_ends
                prev_line_was_section = False
                continue

        if current_command and current_command.meta.get("starter") == "IF" and keyword in {"THEN", "ELSE"}:
            append_to_command(current_command, line, line_no)
            if line_ends:
                _add_command_node(
                    line,
                    line_no,
                    current_div,
                    current_sec,
                    current_para,
                    in_procedure,
                    command_stack,
                    by_kind,
                    anomalies,
                    include_node=False,
                )
                current_command = None
            continue

        if END_LABEL_RE.match(line) or keyword.startswith("END-") or ender:
            is_structural = True
        elif branch:
            is_structural = True
        elif keyword in COMMAND_KEYWORDS or keyword in BLOCK_STARTERS:
            include_node = True

        if current_command and not include_node and keyword == "TO":
            append_to_command(current_command, line, line_no)
            if line_ends:
                _add_command_node(
                    line,
                    line_no,
                    current_div,
                    current_sec,
                    current_para,
                    in_procedure,
                    command_stack,
                    by_kind,
                    anomalies,
                    include_node=False,
                )
                current_command = None
            continue

        allow_para = prev_line_dot or prev_line_was_section
        para_inline = PARA_INLINE_RE.match(line) if allow_para else None
        if para_inline:
            para_name = para_inline.group(1)
            if para_name in COMMAND_KEYWORDS or para_name in BLOCK_STARTERS or para_name.startswith("END-"):
                para_inline = None
            else:
                if current_para:
                    current_para.end_line = prev_line_no(line_no)
                inline_text = para_inline.group(2)
                current_para = Node(kind="PARAGRAPH", name=para_name, start_line=safe_line_no(line_no), end_line=safe_line_no(line_no))
                current_para.division = current_div.name if current_div else None
                current_para.section = current_sec.name if current_sec else None
                current_para.paragraph = para_name
                if current_sec:
                    current_sec.add_child(current_para)
                elif current_div:
                    current_div.add_child(current_para)
                else:
                    root.add_child(current_para)
                by_kind["PARAGRAPH"].append(current_para)
                current_command = None

                _add_command_node(
                    inline_text,
                    line_no,
                    current_div,
                    current_sec,
                    current_para,
                    in_procedure,
                    command_stack,
                    by_kind,
                    anomalies,
                )
                continue

        para_only = PARA_ONLY_RE.match(line) if allow_para else None
        if para_only:
            para_name = para_only.group(1)
            if para_name in COMMAND_KEYWORDS or para_name in BLOCK_STARTERS or para_name.startswith("END-"):
                para_only = None
            else:
                if current_para:
                    current_para.end_line = prev_line_no(line_no)
                current_para = Node(kind="PARAGRAPH", name=para_name, start_line=safe_line_no(line_no), end_line=safe_line_no(line_no))
                current_para.division = current_div.name if current_div else None
                current_para.section = current_sec.name if current_sec else None
                current_para.paragraph = para_name
                if current_sec:
                    current_sec.add_child(current_para)
                elif current_div:
                    current_div.add_child(current_para)
                else:
                    root.add_child(current_para)
                by_kind["PARAGRAPH"].append(current_para)
                current_command = None
                continue

        if include_node or is_structural:
            node = _add_command_node(
                line,
                line_no,
                current_div,
                current_sec,
                current_para,
                in_procedure,
                command_stack,
                by_kind,
                anomalies,
                include_node=include_node,
            )
            if include_node and node:
                current_command = node
                if line_ends:
                    current_command = None
            else:
                current_command = None
            prev_line_dot = line_ends
            prev_line_was_section = False
            continue

        # Not a command and not structural: ignore (prevents variable/copybook lines from becoming commands)
        if current_command:
            append_to_command(current_command, line, line_no)
            if line_ends:
                _add_command_node(
                    line,
                    line_no,
                    current_div,
                    current_sec,
                    current_para,
                    in_procedure,
                    command_stack,
                    by_kind,
                    anomalies,
                    include_node=False,
                )
                current_command = None
            continue
        prev_line_dot = line_ends
        prev_line_was_section = False
        continue

    close_open_commands("EOF", last_real_line_no)
    if current_para:
        current_para.end_line = last_real_line_no
    if current_sec:
        current_sec.end_line = last_real_line_no

    stats = {
        "divisions": len(by_kind["DIVISION"]),
        "sections": len(by_kind["SECTION"]),
        "paragraphs": len(by_kind["PARAGRAPH"]),
        "commands": len(by_kind["COMMAND"]),
        "variables": len(by_kind["VARIABLE"]),
    }
    root.end_line = cleaned_listing.line_map[-1] if cleaned_listing.line_map else 1

    return TokenizationContext(tree=root, by_kind=by_kind, stats=stats, anomalies=anomalies)


def format_tree(context):
    lines = []

    def walk(node, depth):
        indent = "  " * depth
        name = f" {node.name}" if node.name else ""
        loc = f"[{node.start_line}-{node.end_line}]"
        lines.append(f"{indent}{node.kind}{name} {loc}")
        for child in node.children:
            walk(child, depth + 1)

    walk(context.tree, 0)
    return "\n".join(lines)


def format_tree_verbose(context):
    lines = []

    def node_value(node):
        if node.kind == "COMMAND":
            return node.normalized_text.strip()
        if node.kind == "VARIABLE":
            return node.meta.get("value", "")
        if node.name:
            return node.name
        return ""

    def walk(node, depth):
        indent = "  " * depth
        loc = f"[{node.start_line}-{node.end_line}]"
        parts = [node.kind]
        if node.name:
            parts.append(f"name={node.name}")
        if node.kind == "VARIABLE" and node.meta.get("level"):
            parts.append(f"level={node.meta.get('level')}")
        path = node.path()
        if path:
            parts.append(f"path={path}")
        value = node_value(node)
        if value:
            parts.append(f"value={value!r}")
        lines.append(f"{indent}{' '.join(parts)} {loc}")
        for child in node.children:
            walk(child, depth + 1)

    walk(context.tree, 0)
    return "\n".join(lines)


def _add_command_node(
    line,
    line_no,
    current_div,
    current_sec,
    current_para,
    in_procedure,
    command_stack,
    by_kind,
    anomalies,
    include_node=True,
):
    keywords = _keyword_set(line)
    node = None
    if include_node:
        node = Node(kind="COMMAND", name=_first_keyword(line), start_line=line_no, end_line=line_no)
        node.division = current_div.name if current_div else None
        node.section = current_sec.name if current_sec else None
        node.paragraph = current_para.name if current_para else None
        node.text_lines.append(line)
        node.normalized_text = line
        node.keywords = keywords

        parent = command_stack[-1] if command_stack else current_para or current_sec or current_div
        if parent:
            parent.add_child(node)

        by_kind["COMMAND"].append(node)

    if not in_procedure:
        return

    ender, starter = _detect_ender(line)
    if ender:
        for i in range(len(command_stack) - 1, -1, -1):
            if command_stack[i].meta.get("starter") == starter:
                to_close = command_stack[i:]
                del command_stack[i:]
                for n in to_close:
                    n.end_line = line_no
                    n.closed_by = ender
                break
        else:
            anomalies.append(f"Unmatched {ender} at line {line_no}")

    branch = _detect_branch(line)
    if branch:
        for i in range(len(command_stack) - 1, -1, -1):
            if command_stack[i].meta.get("starter") == "EVALUATE":
                command_stack[i].meta.setdefault("branches", set()).add(branch)
                break

    tokens = keywords
    starter = None
    if "PERFORM" in tokens and _is_inline_perform(line):
        starter = "PERFORM"
    else:
        for s in BLOCK_STARTERS:
            if s in tokens:
                starter = s
                break

    if starter and node:
        node.meta["starter"] = starter
        command_stack.append(node)

    if line.strip().endswith("."):
        while command_stack:
            n = command_stack.pop()
            n.end_line = line_no
            n.closed_by = "DOT"

    return node


def _infer_var_value(line):
    m = VALUE_RE.search(line)
    if m:
        value = m.group(1).strip()
        return value, True
    pic = PIC_RE.search(line)
    if pic:
        pic_text = pic.group(1)
        if re.search(r"[XA]", pic_text):
            return " ", False
        if re.search(r"[9SV]", pic_text):
            return "0", False
    return "", False
