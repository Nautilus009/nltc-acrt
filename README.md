# nltc-acrt (ACRT) COBOL Audit POC for COBOL-IT Listing Diff (Python)

`nltc-acrt` is a lightweight COBOL auditing tool inspired by the legacy ACRT.

It compares **Master** vs **Local/Private** COBOL compilation listing files and reports newly introduced violations
(using the Local-only diff: **Local - Master**).

This design is ideal for:
- **ALM commit gating** (block commit if new violations are introduced)
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


This design is ideal for:
- **ALM commit gating** (block commit if new violations are introduced)
- optional **build/compile integration** (post-compile auditing)

---

## Key Features

- **Python-based** (fast iteration, easy maintenance)
- **External XML rules configuration** (`$ACRT_HOME/CONF/ACRT_RULES.XML`)
  - Rule number
  - Severity: **E** (Error), **W** (Warning), **I** (Info)
  - Description
  - Rule type (`type=...`) and optional parameters (`max`, `required`, `exclude`)
  - Code (regex for `REGEX` rules; empty for non-regex types)
  - Scope: run on **Master listing**, **Local listing**, and/or **Local-only diff** (`on_master/on_local/on_diff`)
  - Run flag (Y/N)
- Applies rules on:
  1) Master listing: `$BUILD_ALM_PATH_BB/target/obj/<name>.lis`
  2) Local listing:  `$BUILD_LOCAL_PATH_BB/target/obj/<name>.lis`
  3) Local-only diff: **(Local - Master)** after stripping comments/blanks
- **Console output** is commit-gate friendly: prints **only diagnostics that appear in Local but not in Master**
- **Per-element status report file** with full details, overwritten each run

---

## Supported COBOL Source Types

Input extensions supported:
- `.cob`
- `.pco`
- `.inc`

---

## Environment Variables

Required:
- `ACRT_HOME` - base directory for config
- `BUILD_ALM_PATH_BB` - Master build base path (listing is under `target/obj/`)
- `BUILD_LOCAL_PATH_BB` - Local/Private build base path (listing is under `target/obj/`)
- `ACRT_EXCLUDE` - optional semicolon-separated filename patterns to skip (e.g., `icd_*.pco;ica_srq104_get_data.cob;`)

Example:
```bash
export ACRT_HOME=/CCA/DEVELOP/<user>/BUILD/DEV/classic_ica
export BUILD_ALM_PATH_BB=/CCA/BUILD/BUILDDEV/DEV/classic_ica
export BUILD_LOCAL_PATH_BB=/CCA/DEVELOP/<user>/BUILD/DEV/classic_ica
export ACRT_EXCLUDE="icd_*.pco;ica_*_driver.cob;"
```

---

## Usage

Print version:
```bash
python3 acrt.py -version
```

Run audit for a single COBOL source element:
```bash
python3 acrt.py src/ica_check_if_subs_active.cob
```

### Exit Codes

- `0` - success (meets ACRT standards)
- `1` - failure (does not meet ACRT standards; thresholds exceeded)
- `2` - usage/config/runtime error

---

## Output

### Console (stdout)

For commit gating, stdout prints only actionable diagnostics that appear in Local but not in Master:
```text
   ica_check_if_subs_active.cob:749: Error: ACRT v0.3.0 Rule 2.28: Do not use STOP RUN
```

### Status report file

A full report is written to the Local build output directory:

- Status file: `$BUILD_LOCAL_PATH_BB/target/obj/<input-file-name>.acrt`

This report includes:
- counts on Master listing
- counts on Local listing
- (Local - Master) deltas
- rule matches (Master, Local, and Local-only diff)
- threshold evaluation and final result

---

## Rules Configuration (`ACRT_RULES.XML`)

Rules are configured in:

- `$ACRT_HOME/CONF/ACRT_RULES.XML`

### Thresholds

Thresholds define when the script fails (based on absolute deltas):

```xml
<Thresholds errors="0" warnings="50" infos="100"/>
```

### Rule Scopes

Each rule can be enabled for:
- Master listing (`on_master="Y"`)
- Local listing (`on_local="Y"`)
- Local-only diff (`on_diff="Y"`)

Some rules are **program-level metrics** (run on Master+Local listings) or **source-level** checks (run on Local only).
Diff-only rules are used to detect newly introduced violations.

### Example Rule (REGEX)

```xml
<Rule number="2.28" severity="E" run="Y"
      type="REGEX"
      on_master="Y" on_local="Y" on_diff="N">
  <Description>Do not use STOP RUN</Description>
  <Code>(?:^|[^A-Z0-9-])STOP\s+RUN(?:[^A-Z0-9-]|$)</Code>
</Rule>
```

---

## Notes

- Listing files are cleaned before diffing/rule matching:
  - blank/whitespace-only lines removed
  - full-line comments removed (lines starting with `*`)
- Source-level rules (e.g., unused sections, GO TO restrictions) analyze the original COBOL source file.

---

## License

Internal POC (NLTC / ICC). Add a license if you plan to publish publicly.
