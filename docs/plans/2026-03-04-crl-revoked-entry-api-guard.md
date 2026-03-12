# CRL Revoked-Entry API Guard (X509_CRL_get0_by_cert fail-closed)

## Goal
Prevent runtime access violations when revocation-entry lookup API (`X509_CRL_get0_by_cert`) is unavailable. Revocation checks must fail with controlled unsupported errors.

## Architecture
- RED: extend `tests/unit/test_crl_revocation_semantics.pas` to temporarily set `X509_CRL_get0_by_cert=nil` and assert controlled error semantics.
- GREEN: minimal guard in `TCRLManagerImpl.TryGetRevokedEntry`:
  - preflight `Assigned(X509_CRL_get0_by_cert)`
  - otherwise `RaiseUnsupported('OpenSSL API X509_CRL_get0_by_cert')`
- Regression: CRL/OCSP focused tests + compile gate.

## Files
- Modify: `tests/unit/test_crl_revocation_semantics.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Expected: missing-API controlled-error assertion fails with `Access violation`.

2. GREEN
- same command as RED.
- Expected: all assertions pass.

3. Regression
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
- `python3 scripts/compile_all_modules.py`

## Execution Log (2026-03-04)

### RED
- Added contract in `tests/unit/test_crl_revocation_semantics.pas`:
  - `missing get0_by_cert api returns controlled error`
  - `missing get0_by_cert api must fail closed`
- RED key output:
  - `[FAIL] missing get0_by_cert api returns controlled error: unexpected message: Access violation`

### GREEN
- Updated `src/fafafa.ssl.cert.advanced.pas` (`TryGetRevokedEntry`) with API preflight guard.
- GREEN result:
  - `Results: 27 passed, 0 failed` (`test_crl_revocation_semantics`)

### Regression
- `test_unit_crl`: PASS
- `test_unit_ocsp`: PASS
- `test_ocsp_crl_interface`: PASS (`26 passed, 0 failed`)
- `compile_all_modules.py`: PASS (`179/179`, `0 failed`, `100.0%`)
