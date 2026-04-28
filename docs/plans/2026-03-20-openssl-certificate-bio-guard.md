# OpenSSL Certificate BIO Guard Plan

**Goal:** Make `TOpenSSLCertificate` public load/save helpers fail safely when required BIO helpers are unavailable, instead of dereferencing nil function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around representative file/memory/save helpers
- change only `src/fafafa.ssl.openssl.certificate.pas`
- preserve current successful certificate behavior when helper capabilities are available
- do not redesign certificate loading/saving or global OpenSSL loader behavior

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_openssl_certificate_bio_contract.pas`
- Reference: `src/fafafa.ssl.openssl.certificate.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`
- Reference: `src/fafafa.ssl.openssl.api.x509.pas`
- Reference: `src/fafafa.ssl.openssl.api.pem.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, X509, and PEM support on the current runtime
  - reads a valid certificate fixture from `tests/certificate/test_certs/signer_cert.pem`
  - prepares a valid `TOpenSSLCertificate` instance for save-path coverage
  - temporarily clears representative helpers such as:
    - `BIO_new_file`
    - `BIO_new_mem_buf`
    - `BIO_new`
    - `BIO_s_mem`
    - `BIO_free`
  - asserts representative public helpers must not raise:
    - `LoadFromFile(...)`
    - `LoadFromPEM(...)`
    - `LoadFromMemory(...)`
    - `SaveToFile(...)`
    - `SaveToPEM`
    - `SaveToDER`
  - asserts they degrade to `False` / empty string / empty bytes
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal BIO guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.certificate.pas`

**Steps:**
- Add local guard checks so file helpers require:
  - `BIO_new_file`
  - `BIO_free`
- Add local guard checks so memory load helpers require:
  - `BIO_new_mem_buf`
  - `BIO_free`
- Add local guard checks so memory save helpers require:
  - `BIO_new`
  - `BIO_s_mem`
  - `BIO_free`
- Keep current success behavior unchanged when helpers are available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_certificate_bio_contract && fpc -B -Fu./src -FUtmp/openssl_certificate_bio_contract -FEtmp/openssl_certificate_bio_contract -otmp/openssl_certificate_bio_contract/test_openssl_certificate_bio_contract tests/test_openssl_certificate_bio_contract.pas && ./tmp/openssl_certificate_bio_contract/test_openssl_certificate_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-openssl-certificate-bio-guard.md src/fafafa.ssl.openssl.certificate.pas tests/test_openssl_certificate_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused certificate helper contract passes without raising
- load/save helpers degrade to `False` / empty string / empty bytes when BIO dependencies are unavailable
