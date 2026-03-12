# KDF InfoPtr Unused Note Alignment

## Goal
Eliminate the non-blocking compiler note `Local variable "InfoPtr" not used` in `src/fafafa.ssl.openssl.api.kdf.pas` with a minimal semantics-preserving change.

## Architecture
- Keep KDF behavior unchanged.
- Remove only unused local variable declaration.

## Scope
- Modify: `src/fafafa.ssl.openssl.api.kdf.pas`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Remove unused `InfoPtr` local variable from `DeriveKeyHKDF`.
2. Focused verification:
   - `fpc -Fu./src tests/openssl/test_openssl_chain_issuer_selection.pas -otmp/test_openssl_chain_issuer_selection && ./tmp/test_openssl_chain_issuer_selection`
3. Regression gate:
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Focused compile has no warnings and no notes.
- `python3 scripts/compile_all_modules.py` reports all modules compiled successfully.
