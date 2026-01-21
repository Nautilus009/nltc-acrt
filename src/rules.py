import os
import sys
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from typing import Dict, List, Tuple

from core import error_exit, parse_required_list, parse_yn


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


def load_rules_config(xml_path: str) -> Tuple[List[Rule], Dict[str, int]]:
    if not xml_path:
        error_exit("Rules config path not set.")

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
    for rule_elem in root.findall(".//Rule"):
        number = rule_elem.get("number", "")
        severity = rule_elem.get("severity", "I").upper()
        rtype = rule_elem.get("type", "REGEX").upper()
        run = parse_yn(rule_elem.get("run"), True)
        on_master = parse_yn(rule_elem.get("on_master"), True)
        on_local = parse_yn(rule_elem.get("on_local"), True)
        on_diff = parse_yn(rule_elem.get("on_diff"), True)

        max_value = int(rule_elem.get("max", "0"))
        req_list = parse_required_list(rule_elem.get("required"))
        exclude_list = parse_required_list(rule_elem.get("exclude"))

        desc_elem = rule_elem.find("Description")
        code_elem = rule_elem.find("Code")

        desc = desc_elem.text.strip() if desc_elem is not None and desc_elem.text else ""
        code = code_elem.text.strip() if code_elem is not None and code_elem.text else ""

        if not number:
            continue

        rules.append(
            Rule(
                number=number,
                severity=severity,
                description=desc,
                code=code,
                run=run,
                rtype=rtype,
                max_value=max_value,
                required_list=req_list,
                exclude_list=exclude_list,
                on_master=on_master,
                on_local=on_local,
                on_diff=on_diff,
            )
        )

    if not rules:
        print(f"Warning: No valid rules loaded from {xml_path}", file=sys.stderr)

    return rules, thresholds
