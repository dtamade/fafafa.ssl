# Certificate Utils Conversion BIO Guard Plan

**Goal:** Make `TCertificateUtils` conversion/fingerprint public helpers fail according to their existing contracts when required BIO helpers are unavailable, instead of dereferencing nil function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around PEM/DER conversion and fingerprint helpers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful conversion/fingerprint behavior when helper capabilities are available
- do not redesign certificate generation, chain verification, or `GetInfo(...)`

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_cert_utils_conversion_bio_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`
- Reference: `src/fafafa.ssl.openssl.api.pem.pas`
- Reference: `src/fafafa.ssl.openssl.api.x509.pas`

**Steps:**
- Write a focused contract test that:
  - initializes the OpenSSL library on the current runtime
  - loads a valid certificate PEM fixture and warms DER/fingerprint outputs before niling helpers
  - temporarily clears representative read-path helpers:
    - `BIO_new_mem_buf`
    - `BIO_free`
  - temporarily clears representative write-path helpers:
    - `BIO_new`
    - `BIO_s_mem`
    - `BIO_free`
  - asserts these non-exception helpers must not raise and must degrade cleanly:
    - `TCertificateUtils.PEMToDER(...)`
    - `TCertificateUtils.DERToPEM(...)`
  - asserts these exception-style helpers must fail with controlled certificate exceptions:
    - `TCertificateUtils.GetFingerprint(...)`
  - asserts the Try wrappers do not raise and return `False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal BIO guards

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add local guard checks so PEM read helpers require:
  - `BIO_new_mem_buf`
  - `PEM_read_bio_X509`
  - `BIO_free`
- Add local guard checks so PEM write helpers require:
  - `BIO_new`
  - `BIO_s_mem`
  - `PEM_write_bio_X509`
  - `BIO_free`
- Preserve current contracts:
  - `PEMToDER(...)` returns empty bytes on helper unavailability
  - `DERToPEM(...)` returns empty string on helper unavailability
  - `GetFingerprint(...)` raises controlled `ESSLCertError`
  - Try wrappers continue to return `False`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_conversion_bio_contract && fpc -B -Fu./src -FUtmp/cert_utils_conversion_bio_contract -FEtmp/cert_utils_conversion_bio_contract -otmp/cert_utils_conversion_bio_contract/test_cert_utils_conversion_bio_contract tests/test_cert_utils_conversion_bio_contract.pas && ./tmp/cert_utils_conversion_bio_contract/test_cert_utils_conversion_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-cert-utils-conversion-bio-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_conversion_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused `TCertificateUtils` conversion/fingerprint contract passes without `EAccessViolation`
- conversion helpers degrade cleanly and fingerprint helpers raise controlled exceptions when BIO dependencies are unavailable
