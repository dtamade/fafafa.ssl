# CRL Revoked Entry Nil-Handle Fail-Closed + Whitespace PEM Contracts

## Goal
Harden CRL revocation lookup and PEM boundary semantics:
- revocation query must fail closed for `ACert=nil`,
- revocation query must fail closed for certificate objects with `X509Handle=nil` (no AV),
- whitespace-only PEM payload keeps explicit invalid-data semantics and reload-state clearing.

## Architecture
- RED: extend `tests/unit/test_crl_revocation_semantics.pas`:
  - add `ACert=nil` revocation-query contract,
  - add test double implementing `ICertificateEx` with `GetX509Handle=nil`,
  - add whitespace-only PEM contracts for both initial load and reload-state clearing.
- GREEN: minimal source fix in `src/fafafa.ssl.cert.advanced.pas`:
  - in `TCRLManagerImpl.TryGetRevokedEntry`, add explicit guards:
    - `if not Assigned(ACert) then RaiseUnsupported('Certificate handle access')`
    - `if not Assigned(LCertEx.X509Handle) then RaiseUnsupported('Certificate handle access')`
  - preserve existing behavior for valid certificate handles.

## Files
- Modify: `tests/unit/test_crl_revocation_semantics.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Expected: nil-handle contract fails with `Access violation` in current code.

2. GREEN
- same command as RED.
- Expected: all contracts pass with controlled `Certificate handle access` semantic.

3. Regression
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
- `python3 scripts/compile_all_modules.py`

## Execution Log (2026-03-05)

### RED
- Modified `tests/unit/test_crl_revocation_semantics.pas`:
  - Added `TNilX509Certificate` test double (`GetX509Handle=nil`).
  - Added contracts:
    - `nil cert returns controlled certificate-access error`
    - `nil x509 handle returns controlled certificate-access error`
    - `whitespace-only crl pem returns invalid-data semantic`
    - `whitespace-only crl pem reload returns invalid-data semantic`
    - reload-state clear checks for whitespace payload.
- RED key output:
  - `[FAIL] nil x509 handle returns controlled certificate-access error: unexpected message: Access violation`
  - `Results: 52 passed, 1 failed`

### GREEN
- Modified `src/fafafa.ssl.cert.advanced.pas`:
  - `TryGetRevokedEntry` now guards both `ACert=nil` and `X509Handle=nil`.
- GREEN result:
  - `Results: 53 passed, 0 failed` (`test_crl_revocation_semantics`)

### Regression
- `test_unit_crl`: PASS (`CRL TEST COMPLETE`)
- `test_unit_ocsp`: PASS (`OCSP TEST COMPLETE`)
- `test_ocsp_crl_interface`: PASS (`26 passed, 0 failed`)
- `compile_all_modules.py`: PASS (`179/179`, `0 failed`, `100.0%`)
