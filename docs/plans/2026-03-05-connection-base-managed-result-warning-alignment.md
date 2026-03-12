# Connection Base Managed-Result Warning Alignment

## Goal
Eliminate managed-result initialization warnings in `src/fafafa.ssl.connection.base.pas` with minimal behavior-preserving initialization changes.

## Scope
- Modify: `src/fafafa.ssl.connection.base.pas`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Replace `FillChar(Result, ...)` on managed-record returns with `Result := Default(...)`.
2. Replace `SetLength(Result, 0)` with explicit `Result := nil` for `TBytes` return.
3. Focused verification:
   - `fpc -Fu./src tests/openssl/test_openssl_chain_issuer_selection.pas -otmp/test_openssl_chain_issuer_selection && ./tmp/test_openssl_chain_issuer_selection`
4. Regression gate:
   - `python3 scripts/compile_all_modules.py`
