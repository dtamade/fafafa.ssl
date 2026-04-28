# PKCS7 Helper BIO Guard Plan

**Goal:** Make PKCS7 helper entrypoints fail safely when required BIO constructors / sinks / cleanup helpers are unavailable, instead of dereferencing nil BIO function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for representative PKCS7 input/output helper paths
- change only `src/fafafa.ssl.openssl.api.pkcs7.pas`
- preserve existing behavior when BIO helpers are available
- do not redesign PKCS7 loading or global BIO loading

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_pkcs7_helper_bio_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.pkcs7.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`
- Reference: `tests/fixtures/p2/pkcs7/pkcs7_signed_attached_v1.der`
- Reference: `tests/certificate/test_certs/*`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, PKCS7, PEM, EVP, and stack support on the current runtime
  - uses repository test certs and the attached PKCS7 fixture
  - temporarily clears `BIO_new_mem_buf` / `BIO_free` and asserts representative input-BIO helpers do not raise
  - temporarily clears output BIO dependencies and asserts representative sign/verify/encrypt/decrypt helpers do not raise
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal BIO guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.pkcs7.pas`

**Steps:**
- Add early-return guards so helpers require the BIO functions they dereference:
  - `BIO_new_mem_buf`
  - `BIO_new`
  - `BIO_s_mem`
  - `BIO_free`
  - `BIO_read`
- Keep successful PKCS7 behavior unchanged when those BIO helpers are available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/pkcs7_bio_contract && fpc -B -Fu./src -FUtmp/pkcs7_bio_contract -FEtmp/pkcs7_bio_contract -otmp/pkcs7_bio_contract/test_pkcs7_helper_bio_contract tests/test_pkcs7_helper_bio_contract.pas && ./tmp/pkcs7_bio_contract/test_pkcs7_helper_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-pkcs7-helper-bio-guard.md src/fafafa.ssl.openssl.api.pkcs7.pas tests/test_pkcs7_helper_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused PKCS7 helper contract passes without raising
- helper entrypoints degrade to `nil` / `False` when BIO dependencies are unavailable
