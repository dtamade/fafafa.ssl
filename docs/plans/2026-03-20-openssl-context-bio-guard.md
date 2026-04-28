# OpenSSL Context BIO Guard Plan

**Goal:** Make `TOpenSSLContext` BIO-backed certificate/private-key loading entrypoints fail with controlled SSL exceptions when required BIO helpers are unavailable, instead of dereferencing nil function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around representative context load helpers
- change only `src/fafafa.ssl.openssl.context.pas`
- preserve current successful context loading behavior when helper capabilities are available
- do not redesign context initialization, PKCS#11 flow, or OpenSSL loader behavior

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_openssl_context_bio_contract.pas`
- Reference: `src/fafafa.ssl.openssl.context.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`
- Reference: `src/fafafa.ssl.openssl.api.pem.pas`
- Reference: `src/fafafa.ssl.openssl.api.x509.pas`
- Reference: `src/fafafa.ssl.openssl.api.evp.pas`

**Steps:**
- Write a focused contract test that:
  - initializes the OpenSSL library and creates a real `ISSLContext`
  - uses valid certificate/private-key PEM fixtures from `tests/certificate/test_certs`
  - temporarily clears representative BIO helpers such as:
    - `BIO_new_file`
    - `BIO_new_mem_buf`
    - `BIO_free`
  - asserts these exception-based context helpers must not raise `EAccessViolation`:
    - `LoadCertificate(AStream)`
    - `LoadCertificatePEM(...)`
    - `LoadPrivateKey(const AFileName, const APassword)`
    - `LoadPrivateKey(AStream, ...)`
    - `LoadPrivateKeyPEM(...)`
  - asserts they fail with controlled SSL exception types instead
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal BIO guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.context.pas`

**Steps:**
- Add local guard checks so memory-based context certificate/private-key helpers require:
  - `BIO_new_mem_buf`
  - `BIO_free`
- Add local guard checks so password/BIO file private-key loading requires:
  - `BIO_new_file`
  - `BIO_free`
- Preserve the existing exception-oriented contract:
  - missing capabilities should raise controlled `ESSL*Exception`
  - successful context loading behavior should remain unchanged

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_context_bio_contract && fpc -B -Fu./src -FUtmp/openssl_context_bio_contract -FEtmp/openssl_context_bio_contract -otmp/openssl_context_bio_contract/test_openssl_context_bio_contract tests/test_openssl_context_bio_contract.pas && ./tmp/openssl_context_bio_contract/test_openssl_context_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-openssl-context-bio-guard.md src/fafafa.ssl.openssl.context.pas tests/test_openssl_context_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused OpenSSL context BIO contract passes without `EAccessViolation`
- BIO-backed context load helpers fail with controlled SSL exceptions when BIO dependencies are unavailable
