# PKCS Helper BIO Guard Plan

**Goal:** Make PKCS helper entrypoints fail safely when required BIO constructors / cleanup helpers are unavailable, instead of dereferencing nil BIO function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for representative PKCS file/memory helper paths
- change only `src/fafafa.ssl.openssl.api.pkcs.pas`
- preserve existing behavior when BIO helpers are available
- do not redesign PKCS loading or global BIO loading

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_pkcs_helper_bio_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.pkcs.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, and PKCS support on the current runtime
  - temporarily clears `BIO_new_file` / `BIO_free` and asserts a representative file helper does not raise
  - temporarily clears memory BIO dependencies and asserts representative sign/verify helpers do not raise
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal BIO guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.pkcs.pas`

**Steps:**
- Add early-return guards so file helpers require:
  - `BIO_new_file`
  - `BIO_free`
- Add early-return guards so memory helpers require the BIO constructors / sinks they dereference:
  - `BIO_new_mem_buf`
  - `BIO_new`
  - `BIO_s_null`
  - `BIO_free`
- Keep successful PKCS behavior unchanged when those BIO helpers are available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/pkcs_bio_contract && fpc -B -Fu./src -FUtmp/pkcs_bio_contract -FEtmp/pkcs_bio_contract -otmp/pkcs_bio_contract/test_pkcs_helper_bio_contract tests/test_pkcs_helper_bio_contract.pas && ./tmp/pkcs_bio_contract/test_pkcs_helper_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-pkcs-helper-bio-guard.md src/fafafa.ssl.openssl.api.pkcs.pas tests/test_pkcs_helper_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused PKCS helper contract passes without raising
- helper entrypoints degrade to `False` / `nil` when BIO dependencies are unavailable
