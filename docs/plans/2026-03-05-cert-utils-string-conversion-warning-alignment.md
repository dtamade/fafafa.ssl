# Certificate Utils String Conversion Warning Alignment

## Goal
Eliminate implicit string conversion warnings in `src/fafafa.ssl.cert.utils.pas` by using explicit conversion at encoding boundaries, without changing runtime behavior.

## Scope
- Modify: `src/fafafa.ssl.cert.utils.pas`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Replace implicit string conversions with explicit casts at warning points:
   - `ConvertFormat`
   - `LoadFromFile`
   - `SaveToFile`
2. Focused verification:
   - `fpc -Fu./src tests/openssl/test_openssl_chain_issuer_selection.pas -otmp/test_openssl_chain_issuer_selection && ./tmp/test_openssl_chain_issuer_selection`
   - Confirm warning lines in `fafafa.ssl.cert.utils.pas` are not present in output.
3. Regression gate:
   - `python3 scripts/compile_all_modules.py`
