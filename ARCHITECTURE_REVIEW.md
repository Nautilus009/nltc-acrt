# ACRT Architecture Review

## Overall Assessment

The ACRT codebase is well-structured for a ~1,860-line Python project. The module
decomposition follows a clean pipeline pattern, the data structures are appropriate,
and the external XML configuration is a good design choice. There is no unnecessary
abstraction or over-engineering. The codebase is fit for its stated purpose as a POC
COBOL audit tool for ALM commit gating.

---

## What Works Well

### 1. Clean pipeline architecture

The data flow is linear and easy to follow:

```
CLI (acrt.py) -> Runner (runner.py) -> [listing -> diff -> tokenize -> rules -> report]
```

Each module owns a single responsibility. There are no circular dependencies.
The `runner.py` orchestrator ties everything together in a single `run_acrt()`
function that reads top-to-bottom.

### 2. External XML rule configuration

Rules are data-driven via `ACRT_RULES.XML`. Adding a new REGEX-type rule requires
zero code changes. The `on_master`/`on_local`/`on_diff` scope flags and the `run`
toggle give operators fine control.

### 3. Appropriate data structures

`Node`, `CleanedListing`, `TokenizationContext`, `Finding`, and `Rule` are all
`@dataclass` types with clear fields. The `by_kind` index on `TokenizationContext`
gives O(1) lookup by node type, which is efficient for rule evaluation.

### 4. Diff-based commit gating

The Master vs Local comparison design means only *newly introduced* violations are
reported. This is the correct approach for incremental enforcement on legacy codebases.

### 5. Atomic writes and safe output

Report files are written atomically via `tempfile.mkstemp` + `os.replace`
(`util.py:30`), preventing partial writes.

### 6. Minimal dependencies

The project uses only Python stdlib. No third-party packages needed. This is ideal
for deployment in restricted enterprise environments.

---

## Issues and Observations

### Issue 1: README has a duplicated section (Low - Documentation)

The README contains the same content twice. Lines 1-43 and lines 44-78 repeat the
"Key Features" and "Supported COBOL Source Types" sections. The first occurrence
(lines 1-43) is an older version with less detail; the second (lines 44-78) is the
current version. The first block also has an unclosed code fence at line 42-43.

**Impact:** Confusing for new contributors. Fix by removing lines 1-43 (the stale section).

### Issue 2: Variable name shadowing in `_run_unused_sections` (Medium - Bug Risk)

In `rules.py:338-348`, the `_run_unused_sections` method uses `target` as both a
parameter name (the scope label: "MASTER"/"LOCAL"/"DIFF") and as a local variable
for the PERFORM/GO TO target name:

```python
def _run_unused_sections(self, rule, context, target, element_name, file_stem):
    ...
    m = re.search(r"\bPERFORM\s+([A-Z0-9_-]+)", text)
    if m:
        target = m.group(1)   # shadows the parameter!
```

After this reassignment, the `target` used in the `Finding` constructor at line 362
will be the last PERFORM target name instead of "MASTER"/"LOCAL"/"DIFF". This
produces incorrect findings metadata.

### Issue 3: `Finding` boilerplate is highly repetitive (Low - Maintainability)

Every rule handler constructs `Finding(rule_number=rule.number, severity=rule.severity,
description=rule.description, rule_type=rule.type, target=target, element=element_name, ...)`.
This 8-field constructor appears ~20 times across `rules.py`. A factory method like
`_finding(rule, target, element_name, line, path, snippet)` would reduce duplication
and prevent copy-paste errors.

### Issue 4: `tokenize.py` trailing import (Low - Code Quality)

Line 610: `from .util import extract_program_id` appears at the bottom of
`tokenize.py`, after all function definitions. This import is also used at line 216
inside the `tokenize()` function. It works but violates PEP 8 (imports at top of file).
It appears this was added to resolve a circular import.

### Issue 5: `lock.py` is a no-op stub (Low - Documentation Gap)

The README mentions "Safe report appends using Linux advisory locking (`fcntl.flock`)"
but `lock.py` is just a context manager that yields without doing anything, and it is
not imported anywhere. The report is now written via `atomic_write` (overwrite, not
append), so the locking claim in the README is stale. Either implement the locking or
remove the claim from the README.

### Issue 6: Report says "does not meet standards" even on OK status (Low - Bug)

In `report.py:182`:

```python
f"RESULT: {status} - the following {element} program does not meet ACRT v{version} standards."
```

This line always says "does not meet" regardless of whether `status` is "OK" or "ERROR".
When status is OK, the message is misleading.

### Issue 7: Test comparison is fragile on username (Low - Testing)

The test script compares report output against golden files, but the report embeds
the current username via `getpass.getuser()`. The test normalizes timestamps but not
usernames. Running under a different user causes false failures.

### Issue 8: No unit tests for individual modules (Medium - Testing)

The only test is an end-to-end shell script. There are no unit tests for `tokenize.py`,
`rules.py`, `diff.py`, `config.py`, or `listing.py`. For a rule engine with 15
distinct rule types, this is a risk.

### Issue 9: `_run_program_id_match` re-reads the source file (Low - Efficiency)

In `rules.py:231-241`, `_run_program_id_match` conditionally re-reads and re-parses
the original source file. The source is already parsed in `runner.py:66-73` and
stored in `context.meta["source_program_id"]`. The fallback re-read is unnecessary.

### Issue 10: `SequenceMatcher` for line mapping (Observation)

The `clean_listing` function uses `difflib.SequenceMatcher` to map listing lines back
to source lines. This is O(n*m) in the worst case. For typical COBOL programs (hundreds
to low thousands of lines), this is fine. For very large programs (10K+), it could slow
down. Worth monitoring but not an issue at current scale.

---

## Architecture Diagram

```
                  ACRT_RULES.XML
                       |
                   config.py
                  (Thresholds, Rules)
                       |
  .cob/.pco/.inc       |     master.lis    local.lis
       |               |         |              |
       v               v         v              v
    acrt.py ------> runner.py --+-- listing.py -+
    (CLI)         (orchestrator) |  (read+clean) |
                       |         v              v
                       |      diff.py --------->|
                       |    (Local - Master)    |
                       |         |              |
                       v         v              v
                  tokenize.py  tokenize.py  tokenize.py
                  (master AST) (local AST)  (diff AST)
                       |         |              |
                       +----+----+--------------+
                            |
                       rules.py
                    (RuleEngine x 15 rules)
                            |
                       report.py
                    (text report + console)
                            |
                       util.py
                    (atomic_write)
```

---

## Summary

| Category | Rating |
|---|---|
| Module decomposition | Good |
| Data flow clarity | Good |
| Configuration design | Good |
| Code simplicity | Good (minor repetition in rules.py) |
| Test coverage | Needs improvement |
| README accuracy | Needs cleanup (duplication, stale claims) |
| Bug risk | Variable shadowing in `_run_unused_sections`; misleading OK message |

The architecture is appropriate for the project's scope. The most impactful items
to address are the `target` variable shadowing bug (#2), the misleading OK/ERROR
report message (#6), and adding unit tests (#8). The README cleanup (#1, #5) is
quick to do and improves onboarding.
