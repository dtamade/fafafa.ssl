# CRL Free API Guard (X509_CRL_free fail-closed)

## Goal
Prevent access violations when replacing loaded CRL state while `X509_CRL_free` is unavailable. Missing free API must surface as controlled unsupported error.

## Architecture
- RED: extend `tests/unit/test_crl_revocation_semantics.pas`:
  - load fixture CRL
  - temporarily set `X509_CRL_free=nil`
  - trigger reload path (`LoadFromPEM`) that frees old CRL
  - assert controlled error (not AV)
- GREEN: minimal source hardening in `src/fafafa.ssl.cert.advanced.pas`:
  - `ParseCRL`: before freeing existing `FCRL`, check `Assigned(X509_CRL_free)`.
  - `Destroy`: only call `X509_CRL_free` when function pointer is assigned.

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
- Added contracts in `tests/unit/test_crl_revocation_semantics.pas`:
  - `missing crl_free api returns controlled error`
  - `missing crl_free api must fail closed`
- RED key output:
  - `[FAIL] missing crl_free api returns controlled error: unexpected message: Access violation`

### GREEN
- Updated `src/fafafa.ssl.cert.advanced.pas`:
  - `ParseCRL`: guard `X509_CRL_free` before free old CRL.
  - `Destroy`: conditional free only when API pointer exists.
- GREEN result:
  - `Results: 29 passed, 0 failed` (`test_crl_revocation_semantics`)

### Regression
- `test_unit_crl`: PASS
- `test_unit_ocsp`: PASS
- `test_ocsp_crl_interface`: PASS (`26 passed, 0 failed`)
- `compile_all_modules.py`: PASS (`179/179`, `0 failed`, `100.0%`)
