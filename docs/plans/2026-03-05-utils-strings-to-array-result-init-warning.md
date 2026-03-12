# Utils StringsToArray Result Init Warning

## Goal
Eliminate the managed-result initialization warning in `src/fafafa.ssl.utils.pas` (`StringsToArray`) with a minimal semantics-preserving change.

## Scope
- Modify: `src/fafafa.ssl.utils.pas`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Use explicit `Result := nil` initialization at function entry.
2. Focused verification:
   - `fpc -Fu./src tests/openssl/test_openssl_chain_issuer_selection.pas -otmp/test_openssl_chain_issuer_selection && ./tmp/test_openssl_chain_issuer_selection`
3. Regression gate:
   - `python3 scripts/compile_all_modules.py`
