# P2 PKCS7 Skip-Debt Reduction (Encrypt Path)

## Goal
Reduce P0 skipped-path debt in `tests/certificate/test_p2_pkcs7.pas` by converting the outdated encrypt skip into a real executable assertion.

## Architecture
- Keep production bindings unchanged.
- Convert `Test_13_PKCS7_Encrypt_Basic` from capability skip to runtime execution using already available stack + PKCS7 APIs.
- Keep one known skip (`Test_10` BIO full-structure dependency) and harden summary checks to validate accounting consistency instead of requiring mandatory skips.

## Files
- Modify: `tests/certificate/test_p2_pkcs7.pas`

## Step-by-step
1. Baseline (RED against target behavior)
- Command:
  - `fpc -Fu./src tests/certificate/test_p2_pkcs7.pas -otmp/test_p2_pkcs7 && ./tmp/test_p2_pkcs7`
- Expected:
  - `Test_13` is `[SKIP]`
  - Summary contains `Skipped:      2`

2. Implement minimal test change (GREEN)
- Update `Test_13_PKCS7_Encrypt_Basic`:
  - Guard runtime availability of `OPENSSL_sk_new_null`/`OPENSSL_sk_push`/`OPENSSL_sk_free`
  - Build recipient stack with generated cert
  - Call `PKCS7_encrypt(..., EVP_aes_256_cbc(), PKCS7_BINARY)`
  - Assert pass/fail and free temporary resources
- Update summary checks:
  - Validate `TestsSkipped` equals category breakdown total
  - Validate `SkipStackPartialCount <= SkipCapability`

3. Regression
- Command:
  - `fpc -Fu./src tests/certificate/test_p2_pkcs7.pas -otmp/test_p2_pkcs7 && ./tmp/test_p2_pkcs7`
- Expected:
  - `Test_13` becomes `[PASS]`
  - Summary contains `Skipped:      1`
  - `Failed:       0`

## Expected outputs
- `PKCS7 encrypt basic operation` transitions from skipped to pass.
- No regression in existing sections and teardown.

## Execution Record (2026-03-04)
- Baseline:
  - Observed `[SKIP] PKCS7 encrypt basic operation - stack API not fully implemented`
  - Observed `Skipped:      2`
- After change:
  - Observed `[PASS] PKCS7 encrypt basic operation`
  - Observed `Skipped:      1`
  - Observed `Failed:       0`
