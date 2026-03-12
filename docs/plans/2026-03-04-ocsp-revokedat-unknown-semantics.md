# OCSP RevokedAt Unknown Semantics (No `Now` Placeholder)

## Goal
Remove `TOCSPClient.CheckCertificate` placeholder behavior that sets `RevokedAt := Now` for revoked status. When revocation timestamp is unavailable, `RevokedAt` must remain explicit unknown (`0`).

## Architecture
- Add deterministic OCSP status hook to drive `TOCSPClient` status mapping without network dependency.
- Add unit contract test covering revoked/good/unknown/error status mapping.
- Keep implementation minimal and behavior-safe:
  - revoked status sets `Status=ocspRevoked`
  - `RevokedAt=0` unless real metadata exists (currently unavailable in this path)

## Files
- Modify: `src/fafafa.ssl.cert.advanced.pas`
- Add: `tests/unit/test_ocsp_client_semantics.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- Expected: revoked timestamp assertion fails under current placeholder.

2. GREEN
- Same command as RED.
- Expected: all contracts pass.

3. Regression
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`


## Execution Log (2026-03-04)

### RED
- Added test seam in `src/fafafa.ssl.cert.advanced.pas`:
  - `TOCSPStatusResolver`
  - `OCSPStatusResolverHook` (default `nil`, production behavior unchanged)
- Added contract test: `tests/unit/test_ocsp_client_semantics.pas`
  - Drives `TOCSPClient.CheckCertificate` statuses via hook.
  - Asserts revoked status keeps `RevokedAt=0` when timestamp is unavailable.

- Command:
  - `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- RED result (key):
  - `[FAIL] revoked revokedAt unknown semantics: expected 0 got 46085...`

### GREEN
- Modified `src/fafafa.ssl.cert.advanced.pas`
  - Removed revoked-path placeholder assignment `Result.RevokedAt := Now`.
  - Revoked status now keeps explicit unknown timestamp semantic (`0`).

- Command:
  - `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- GREEN result:
  - `Results: 13 passed, 0 failed`

### Regression
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
  - PASS (`OCSP TEST COMPLETE`)
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
  - PASS (`Results: 26 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
