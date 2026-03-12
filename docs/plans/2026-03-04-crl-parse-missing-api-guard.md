# CRL Parse Missing OpenSSL API Guard (fail-closed, no AV)

## Goal
Ensure CRL parsing fails with controlled exceptions when required OpenSSL function pointers are unavailable, instead of raising runtime access violations.

## Architecture
- RED: extend `tests/unit/test_crl_revocation_semantics.pas` with a contract that temporarily sets `BIO_new_mem_buf=nil` and verifies error handling is controlled.
- GREEN: add preflight guards in `TCRLManagerImpl.ParseCRL` for:
  - `BIO_new_mem_buf`
  - `PEM_read_bio_X509_CRL`
  - `BIO_free`
- Keep behavior minimal: raise `RaiseUnsupported(...)` before any pointer invocation.

## Files
- Modify: `tests/unit/test_crl_revocation_semantics.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Expected: controlled-error assertion fails with `Access violation`.

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
- Added contract `missing BIO api returns controlled error` in `tests/unit/test_crl_revocation_semantics.pas`.
- RED key output:
  - `[FAIL] missing BIO api returns controlled error: unexpected message: Access violation`

### GREEN
- Updated `src/fafafa.ssl.cert.advanced.pas` (`ParseCRL`) to guard missing OpenSSL APIs before usage.
- GREEN result:
  - `Results: 25 passed, 0 failed` (`test_crl_revocation_semantics`)

### Regression
- `test_unit_crl`: PASS
- `test_unit_ocsp`: PASS
- `test_ocsp_crl_interface`: PASS (`26 passed, 0 failed`)
- `compile_all_modules.py`: PASS (`179/179`, `0 failed`, `100.0%`)
