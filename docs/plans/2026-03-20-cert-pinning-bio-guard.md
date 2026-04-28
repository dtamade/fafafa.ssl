# Certificate Pinning BIO Guard Plan

**Goal:** Make `TPinValidator` public key pinning entrypoints stop dereferencing unused BIO helpers when extracting SPKI hashes, so pin validation continues to work safely even if representative `BIO_*` helpers are unavailable.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around the public pin-validation entrypoints
- change only `src/fafafa.ssl.cert.pinning.pas`
- preserve successful public key pinning behavior when capabilities needed for SPKI hashing are available
- do not redesign certificate pinning policy, chain traversal, or broader certificate helpers

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_cert_pinning_bio_contract.pas`
- Reference: `src/fafafa.ssl.cert.pinning.pas`
- Reference: `src/fafafa.ssl.cert.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`
- Reference: `src/fafafa.ssl.openssl.api.x509.pas`
- Reference: `src/fafafa.ssl.openssl.api.evp.pas`

**Steps:**
- Write a focused contract test that:
  - initializes the OpenSSL library on the current runtime
  - prepares a real self-signed certificate and a matching public-key pin before niling helper pointers
  - temporarily clears representative unused helpers:
    - `BIO_new`
    - `BIO_s_mem`
    - `BIO_free`
  - asserts these public entrypoints must not raise `EAccessViolation`:
    - `TPinValidator.ValidateCertificate(...)`
    - `TPinValidator.ValidateCertificateChain(...)`
    - `TPinValidatorEx.ValidateCertificateEx(...)`
  - asserts matching pin validation should still succeed because the SPKI extraction path does not actually need BIO storage
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Remove the dead BIO dependency

**Files:**
- Modify: `src/fafafa.ssl.cert.pinning.pas`

**Steps:**
- Remove the unused memory-BIO allocation/cleanup from `ExtractPublicKeyHash(...)`
- Keep SPKI DER encoding on the direct `i2d_PUBKEY(...)` buffer path
- Preserve existing matching and logging behavior for public key pinning

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_pinning_bio_contract && fpc -B -Fu./src -FUtmp/cert_pinning_bio_contract -FEtmp/cert_pinning_bio_contract -otmp/cert_pinning_bio_contract/test_cert_pinning_bio_contract tests/test_cert_pinning_bio_contract.pas && ./tmp/cert_pinning_bio_contract/test_cert_pinning_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-cert-pinning-bio-guard.md src/fafafa.ssl.cert.pinning.pas tests/test_cert_pinning_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused certificate pinning BIO contract passes without `EAccessViolation`
- public key pin validation keeps working when the unrelated BIO helper pointers are unavailable
