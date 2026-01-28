import argparse
import os
import sys

from acrt_pkg import __version__
from acrt_pkg.env import require_env, get_optional_env, EnvError
from acrt_pkg.runner import run_acrt
from acrt_pkg.util import matches_any_pattern
from acrt_pkg.rules import SEVERITY_LABELS


def _parse_args(argv):
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument("-version", action="store_true")
    parser.add_argument("-debug", action="store_true")
    parser.add_argument("-debug-tree", action="store_true")
    parser.add_argument("element", nargs="?")
    return parser.parse_args(argv)


def main(argv=None):
    args = _parse_args(argv or sys.argv[1:])
    if args.version:
        print(__version__)
        return 0

    if not args.element:
        print("Usage: python3 acrt.py src/<element>.<cob|pco|inc>")
        return 2

    element_path = args.element
    element_name = os.path.basename(element_path)
    ext = os.path.splitext(element_name)[1].lower()
    if ext not in {".cob", ".pco", ".inc"}:
        print(f"Unsupported file type: {ext}")
        return 2

    exclude_raw = get_optional_env("ACRT_EXCLUDE", "")
    exclude_patterns = [p for p in exclude_raw.split(";") if p.strip()]
    if exclude_patterns and matches_any_pattern(element_name, exclude_patterns):
        return 0

    try:
        env = {
            "ACRT_HOME": require_env("ACRT_HOME"),
            "BUILD_ALM_PATH_BB": require_env("BUILD_ALM_PATH_BB"),
            "BUILD_LOCAL_PATH_BB": require_env("BUILD_LOCAL_PATH_BB"),
        }
    except EnvError as e:
        print(str(e))
        return 2

    try:
        result = run_acrt(element_path, env, debug=args.debug, debug_tree=args.debug_tree)
    except Exception as e:
        print(f"ACRT error: {e}")
        return 2

    for finding in result.actionable:
        sev = SEVERITY_LABELS.get(finding.severity, finding.severity)
        line = "?" if finding.line is None else finding.line
        print(
            f"   {element_name}:{line}: {sev}: ACRT v{__version__} Rule {finding.rule_number}: {finding.description}"
        )

    return result.exit_code


if __name__ == "__main__":
    sys.exit(main())
