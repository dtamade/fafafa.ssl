# Debug Utils String Conversion Warning Alignment

## Goal
Eliminate implicit string conversion warnings in `src/fafafa.ssl.debug.utils.pas` at UTF-8 read/write boundaries, without behavior changes.

## Scope
- Modify: `src/fafafa.ssl.debug.utils.pas`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Use explicit casts at warning points in:
   - `TSSLMemoryStream.ReadString`
   - `TSSLMemoryStream.WriteString`
2. Focused verification:
   - `fpc -Fu./src tests/openssl/test_openssl_chain_issuer_selection.pas -otmp/test_openssl_chain_issuer_selection && ./tmp/test_openssl_chain_issuer_selection`
3. Regression gate:
   - `python3 scripts/compile_all_modules.py`
