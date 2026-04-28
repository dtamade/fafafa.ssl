# Advanced Certificate CRL BIO Guard Plan

**Goal:** Make `TCRLManagerImpl.LoadFromPEM(...)` fail with a controlled SSL exception when CRL memory-BIO helpers are unavailable, instead of dereferencing a nil `BIO_*` helper and crashing with `EAccessViolation`.

**Architecture:** Keep this batch narrow:

- add one focused CRL load contract test
- change only `src/fafafa.ssl.cert.advanced.pas`
- preserve current successful CRL parsing behavior when BIO/PEM helpers are available
- do not redesign OCSP, PKCS#12, or revocation policy behavior

## Task 1: RED - Reproduce the CRL load helper gap

**Files:**
- Add: `tests/test_cert_advanced_crl_bio_contract.pas`
- Reference: `src/fafafa.ssl.cert.advanced.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`
- Reference: `src/fafafa.ssl.openssl.api.pem.pas`

**Steps:**
- Write a focused contract test that:
  - initializes the OpenSSL library and loads `Core/BIO/X509/PEM`
  - uses a valid CRL PEM fixture and warms a normal `LoadFromPEM(...)` path
  - temporarily clears representative CRL read helpers such as:
    - `BIO_new_mem_buf`
    - `BIO_free`
  - asserts `LoadFromPEM(...)` must not raise `EAccessViolation`
  - asserts it fails with a controlled `ESSLException` instead
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal CRL BIO guard

**Files:**
- Modify: `src/fafafa.ssl.cert.advanced.pas`

**Steps:**
- Add a local helper predicate/guard so CRL PEM parsing requires:
  - `BIO_new_mem_buf`
  - `PEM_read_bio_X509_CRL`
  - `BIO_free`
- Keep the current exception-style contract:
  - missing capability should raise a controlled `ESSLException`
  - valid CRL parsing should remain unchanged when helpers are available

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_advanced_crl_bio_contract && fpc -B -Fu./src -FUtmp/cert_advanced_crl_bio_contract -FEtmp/cert_advanced_crl_bio_contract -otmp/cert_advanced_crl_bio_contract/test_cert_advanced_crl_bio_contract tests/test_cert_advanced_crl_bio_contract.pas && ./tmp/cert_advanced_crl_bio_contract/test_cert_advanced_crl_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-cert-advanced-crl-bio-guard.md src/fafafa.ssl.cert.advanced.pas tests/test_cert_advanced_crl_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused CRL BIO contract passes without `EAccessViolation`
- `LoadFromPEM(...)` raises controlled SSL exceptions when required CRL BIO helpers are unavailable
