# CRL Revocation Reason Decode Accessors Unknown Semantics

## Goal
Harden `ICRLManager.GetRevocationReason` when reason decoding accessors are unavailable.
If reason extension exists but decode APIs are all missing, the result must remain explicit unknown (`''`) instead of synthetic `Unknown(-1)`.

## Architecture
- RED: extend `tests/unit/test_crl_revocation_semantics.pas` with runtime contract:
  - temporarily set these accessors to `nil`:
    - `ASN1_INTEGER_get`
    - `ASN1_INTEGER_get_int64`
    - `ASN1_STRING_length`
    - `ASN1_STRING_get0_data`
  - call `GetRevocationReason`
  - assert result is `''`
- GREEN: minimal fix in `src/fafafa.ssl.cert.advanced.pas`:
  - in `GetRevocationReason`, if reason code remains undecodable (`LReasonCode < 0`), return `''`.

## Files
- Modify: `tests/unit/test_crl_revocation_semantics.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Expected: new decode-accessor unknown assertion fails (`Unknown(-1)`).

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
  - Added contract `revocation reason unknown when decode accessors are missing`.
- RED key output:
  - `[FAIL] revocation reason unknown when decode accessors are missing: expected empty reason got=Unknown(-1)`
  - `Results: 34 passed, 1 failed`

### GREEN
- Modified `src/fafafa.ssl.cert.advanced.pas`:
  - `GetRevocationReason` now returns `''` when `LReasonCode` remains undecodable (`<0`).
- GREEN result:
  - `Results: 35 passed, 0 failed` (`test_crl_revocation_semantics`)

### Regression
- `test_unit_crl`: PASS (`CRL TEST COMPLETE`)
- `test_unit_ocsp`: PASS (`OCSP TEST COMPLETE`)
- `test_ocsp_crl_interface`: PASS (`26 passed, 0 failed`)
- `compile_all_modules.py`: PASS (`179/179`, `0 failed`, `100.0%`)
