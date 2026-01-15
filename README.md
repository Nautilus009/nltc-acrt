# nltc-acrt (ACRT / “Ah'Krat”) – COBOL Audit POC for COBOL-IT Listing Diff (Python)

`nltc-acrt` is a lightweight COBOL auditing tool inspired by the legacy ACRT (pronounced **Ah'Krat**).

It compares **Master** vs **Local/Private** COBOL compilation listing files and reports **only newly introduced violations**
(using the Local-only diff: **Local − Master**).

This design is ideal for:
- **AML commit gating** (block commit if new violations are introduced)
- optional **build/compile integration** (post-compile auditing)

---

## Key Features

- **Python-based** (fast iteration, easy maintenance)
- **External XML rules configuration**
  - Rule number
  - Severity: **E** (Error), **W** (Warning), **I** (Info)
  - Description
  - Regex pattern
  - Run flag (Y/N)
- Applies rules on:
  1) Master listing: `$BUILD_ALM_PATH_BB/target/obj/<name>.lis`
  2) Local listing:  `$BUILD_LOCAL_PATH_BB/target/obj/<name>.lis`
  3) Local-only diff: (2 − 1) after stripping comments/blanks
- **Console output** shows only actionable diagnostics lines (commit-gate friendly)
- **Per-element status report file** with full details + history
- **Safe report appends** using Linux advisory locking (`fcntl.flock`) on the status file

---

## Supported COBOL Source Types

Input extensions supported:
- `.cob`
- `.pco`
- `.inc`

Example:
```bash
python3 acrt.py src/ica_check_if_subs_active.cob
