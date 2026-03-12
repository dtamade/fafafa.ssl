# P2 PKCS7 Skip Debt Reduction - BIO Roundtrip

## Goal
Remove the remaining fixed skip in `tests/certificate/test_p2_pkcs7.pas` (`Test_10_PKCS7_BIO_Operations`) by converting it to a real executable BIO roundtrip assertion.

## Architecture
- Build a real PKCS7 object via `PKCS7_sign` using generated local test cert/key.
- Serialize with `i2d_PKCS7_bio`.
- Deserialize with `d2i_PKCS7_bio`.
- Assert roundtrip object is non-nil; keep cleanup strict.

## Files
- Modify: `tests/certificate/test_p2_pkcs7.pas`

## Steps
1. RED/Refactor test path
- Replace fixed skip in `Test_10_PKCS7_BIO_Operations` with executable roundtrip flow.
- Add forward declarations if needed for cert/key generator helpers.

2. GREEN
- Command:
  - `fpc -Fu./src tests/certificate/test_p2_pkcs7.pas -otmp/test_p2_pkcs7 && ./tmp/test_p2_pkcs7`
- Expected:
  - `Test_10` passes.
  - skipped count decreases from `1` to `0` in this environment.

3. Regression
- Command:
  - `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
- Expected:
  - pass, no side effects from PKCS7 test cleanup.

