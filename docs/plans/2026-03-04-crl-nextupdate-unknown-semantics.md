# CRL NextUpdate Unknown Semantics (No `Now+7` Fallback)

## Goal
Avoid synthesizing fake CRL next-update time when `X509_CRL_get0_nextUpdate` is unavailable. Unknown next-update must be explicit (`0`).

## Architecture
- Extend `tests/unit/test_crl_revocation_semantics.pas` with a runtime contract:
  - temporarily disable `X509_CRL_get0_nextUpdate`
  - load valid CRL fixture
  - assert `GetNextUpdate = 0`
- Minimal fix in `src/fafafa.ssl.cert.advanced.pas`: replace `Now + 7` fallback with `0`.

## Files
- Modify: `tests/unit/test_crl_revocation_semantics.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Expected: new next-update unknown assertion fails.

2. GREEN
- Same as RED.
- Expected: all assertions pass.

3. Regression
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`

