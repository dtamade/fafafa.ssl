# Certificate Advanced String Conversion Warning Alignment

## Goal
Eliminate implicit `AnsiString -> UnicodeString` conversion warnings in `src/fafafa.ssl.cert.advanced.pas` at UTF-8 encoding boundaries, with no runtime behavior change.

## Scope
- Modify: `src/fafafa.ssl.cert.advanced.pas`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Replace `TEncoding.UTF8.GetBytes(...)` call inputs with explicit `UnicodeString(...)` casts at warning sites.
2. Focused verification:
   - `fpc -Fu./src tests/openssl/test_openssl_chain_issuer_selection.pas -otmp/test_openssl_chain_issuer_selection && ./tmp/test_openssl_chain_issuer_selection`
3. Regression gate:
   - `python3 scripts/compile_all_modules.py`
