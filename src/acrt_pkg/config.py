import xml.etree.ElementTree as ET
from dataclasses import dataclass


@dataclass
class Thresholds:
    errors: int
    warnings: int
    infos: int


@dataclass
class Rule:
    number: str
    severity: str
    run: bool
    type: str
    on_master: bool
    on_local: bool
    on_diff: bool
    description: str
    code: str
    attrs: dict


class ConfigError(Exception):
    pass


def _bool_attr(value, default=False):
    if value is None:
        return default
    return value.strip().upper() == "Y"


def load_rules(path):
    try:
        tree = ET.parse(path)
    except ET.ParseError as e:
        raise ConfigError(f"Invalid XML: {e}")

    root = tree.getroot()
    thresholds_el = root.find("Thresholds")
    if thresholds_el is None:
        raise ConfigError("Missing <Thresholds> in rules XML")

    thresholds = Thresholds(
        errors=int(thresholds_el.get("errors", "0")),
        warnings=int(thresholds_el.get("warnings", "0")),
        infos=int(thresholds_el.get("infos", "0")),
    )

    rules = []
    rules_el = root.find("Rules")
    if rules_el is None:
        raise ConfigError("Missing <Rules> in rules XML")

    for rule_el in rules_el.findall("Rule"):
        number = rule_el.get("number", "")
        severity = rule_el.get("severity", "I")
        run = _bool_attr(rule_el.get("run"), True)
        rtype = rule_el.get("type", "REGEX")
        on_master = _bool_attr(rule_el.get("on_master"), False)
        on_local = _bool_attr(rule_el.get("on_local"), False)
        on_diff = _bool_attr(rule_el.get("on_diff"), False)
        description = (rule_el.findtext("Description") or "").strip()
        code = (rule_el.findtext("Code") or "").strip()
        attrs = dict(rule_el.attrib)
        rules.append(
            Rule(
                number=number,
                severity=severity,
                run=run,
                type=rtype,
                on_master=on_master,
                on_local=on_local,
                on_diff=on_diff,
                description=description,
                code=code,
                attrs=attrs,
            )
        )

    return thresholds, rules
