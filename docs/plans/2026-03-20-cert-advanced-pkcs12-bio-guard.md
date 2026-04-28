# Advanced Certificate PKCS12 BIO Guard Plan

**Goal:** Make `TPKCS12Manager` PKCS#12 export/import entrypoints fail safely when required BIO helpers are unavailable, instead of dereferencing nil function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around representative PKCS#12 create/load helpers
- change only `src/fafafa.ssl.cert.advanced.pas`
- preserve current successful PKCS#12 export/import behavior when helper capabilities are available
- do not redesign CRL management, OCSP behavior, or broader certificate utility APIs

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_cert_advanced_pkcs12_bio_contract.pas`
- Reference: `src/fafafa.ssl.cert.advanced.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`
- Reference: `src/fafafa.ssl.openssl.api.pkcs12.pas`
- Reference: `src/fafafa.ssl.cert.pas`

**Steps:**
- Write a focused contract test that:
  - initializes the OpenSSL library on the current runtime
  - prepares a real keypair/certificate object before niling helper pointers
  - warms valid PKCS#12 bytes before niling load-path cleanup helpers
  - temporarily clears representative helpers such as:
    - `BIO_new`
    - `BIO_s_mem`
    - `BIO_new_mem_buf`
    - `BIO_free`
  - asserts `TPKCS12Manager.CreatePKCS12(...)` must not raise `EAccessViolation`
  - asserts `TPKCS12Manager.CreatePKCS12(...)` fails with controlled `ESSLException`
  - asserts `TPKCS12Manager.LoadFromPKCS12(...)` must not raise and must degrade to `False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal BIO guard

**Files:**
- Modify: `src/fafafa.ssl.cert.advanced.pas`

**Steps:**
- Add local guard checks so PKCS#12 export helpers require:
  - `BIO_new`
  - `BIO_s_mem`
  - `BIO_free`
- Add local guard checks so PKCS#12 import helpers require:
  - `BIO_new_mem_buf`
  - `BIO_free`
- Preserve existing contract behavior:
  - `CreatePKCS12(...)` should fail with controlled SSL exceptions
  - `LoadFromPKCS12(...)` should return `False`
  - successful PKCS#12 create/load behavior should remain unchanged

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_advanced_pkcs12_bio_contract && fpc -B -Fu./src -FUtmp/cert_advanced_pkcs12_bio_contract -FEtmp/cert_advanced_pkcs12_bio_contract -otmp/cert_advanced_pkcs12_bio_contract/test_cert_advanced_pkcs12_bio_contract tests/test_cert_advanced_pkcs12_bio_contract.pas && ./tmp/cert_advanced_pkcs12_bio_contract/test_cert_advanced_pkcs12_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-cert-advanced-pkcs12-bio-guard.md src/fafafa.ssl.cert.advanced.pas tests/test_cert_advanced_pkcs12_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused PKCS#12 BIO contract passes without `EAccessViolation`
- PKCS#12 export/import entrypoints degrade according to their public contracts when BIO helpers are unavailable
