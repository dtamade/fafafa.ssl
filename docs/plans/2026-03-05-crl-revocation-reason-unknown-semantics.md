# CRL Revocation Reason Unknown Semantics (missing accessor fail-closed)

## Goal
Align `ICRLManager.GetRevocationReason` degradation behavior with existing unknown-semantics policy:
- when revocation reason accessor API is unavailable at runtime,
- return explicit unknown (`''`) instead of synthesized fallback (`'Unspecified'`).

## Architecture
- RED: extend `tests/unit/test_crl_revocation_semantics.pas` with runtime contract:
  - temporarily set `X509_REVOKED_get_ext_d2i := nil`
  - call `GetRevocationReason`
  - assert result is empty string (`''`)
- GREEN: minimal source fix in `src/fafafa.ssl.cert.advanced.pas`:
  - in `TCRLManagerImpl.GetRevocationReason`, early-exit unknown when `X509_REVOKED_get_ext_d2i` is missing.
  - keep existing reason mapping for normal path unchanged.

## Files
- Modify: `tests/unit/test_crl_revocation_semantics.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Expected: new assertion fails because current behavior returns `Unspecified`.

2. GREEN
- same command as RED.
- Expected: all assertions pass.

3. Regression
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
- `python3 scripts/compile_all_modules.py`

## Execution Log (2026-03-05)

### RED
- Modified `tests/unit/test_crl_revocation_semantics.pas`:
  - Added contract `revocation reason unknown when accessor is missing`.
- RED key output:
  - `[FAIL] revocation reason unknown when accessor is missing: expected empty reason got=Unspecified`
  - `Results: 33 passed, 1 failed`

### GREEN
- Modified `src/fafafa.ssl.cert.advanced.pas`:
  - `GetRevocationReason` now returns unknown (`''`) when `X509_REVOKED_get_ext_d2i` is unavailable.
- GREEN result:
  - `Results: 34 passed, 0 failed` (`test_crl_revocation_semantics`)

### Regression
- `test_unit_crl`: PASS (`CRL TEST COMPLETE`)
- `test_unit_ocsp`: PASS (`OCSP TEST COMPLETE`)
- `test_ocsp_crl_interface`: PASS (`26 passed, 0 failed`)
- `compile_all_modules.py`: PASS (`179/179`, `0 failed`, `100.0%`)
