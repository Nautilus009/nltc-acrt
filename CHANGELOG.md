# Changelog

## 0.4.1 - 2026-02-14
- Fixed MASTER listing line mapping to align with `tests/master/src` when available.
- Fixed Rule 3.23 line attribution on MASTER output to anchor to containing section line.
- Fixed Rule 5.1 to ignore `DECLARATIVES` sections and evaluate only executable PROCEDURE sections.
- Updated Rule 3.23 to accept numeric paragraph prefixes matching numeric section prefixes (for example, `001` under `001-I-O-PROBLEM`).
- Improved spool actionable filtering:
  - MASTER-comparable LOCAL findings are de-duplicated against MASTER using count-aware matching on `(rule_number, path, snippet)`.
  - Rules configured for LOCAL-only (`on_master="N"`) are always displayed on spool.

## 0.4.0
- Initial v0.4.0 rule engine and reporting baseline.
