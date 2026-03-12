# CRL Revoked Date Unknown Semantics (No `Now` Fallback)

## Goal
Prevent `ICRLManager.GetRevokedDate` from returning fake current time when revocation date accessor is unavailable. Unknown revocation date must remain explicit (`0`).

## Architecture
- Add RED assertion in `tests/unit/test_crl_revocation_semantics.pas` by temporarily disabling `X509_REVOKED_get0_revocationDate`.
- Keep existing positive fixture assertions unchanged (real revocation date still extracted when API is available).
- Minimal source fix in `src/fafafa.ssl.cert.advanced.pas`: remove `Now` fallback.

## Files
- Modify: `tests/unit/test_crl_revocation_semantics.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Expected: fail on unknown-semantics assertion (currently returns `Now`).

2. GREEN
- Same command as RED.
- Expected: all assertions pass.

3. Regression
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
- Expected: pass.

