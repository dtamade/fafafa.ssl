# Deprecated Warning Scope Whitelist Contract

## Goal
Prevent future spread of `{$WARN SYMBOL_DEPRECATED OFF}` beyond approved compatibility/bridge locations.

## Architecture
- Add a script contract test that scans `src/` + `tests/` for `SYMBOL_DEPRECATED` warning directives.
- Enforce strict whitelist by file and expected occurrence count.
- Fail fast if any new file introduces deprecated-warning suppression.

## Scope
- Add: `tests/scripts/test_deprecated_warning_scope_whitelist_contract.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Implement strict whitelist script with expected counts.
2. Verify shell syntax:
   - `bash -n tests/scripts/test_deprecated_warning_scope_whitelist_contract.sh`
3. Run contract:
   - `bash tests/scripts/test_deprecated_warning_scope_whitelist_contract.sh`
4. Keep regression gate discipline:
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Script passes only when deprecated warning suppression remains confined to approved files.
- Full module compile remains green.
