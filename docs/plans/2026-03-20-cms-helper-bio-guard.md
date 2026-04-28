# CMS Helper BIO Guard Plan

**Goal:** Make CMS helper entrypoints fail safely when required BIO constructors / sinks / cleanup helpers are unavailable, instead of dereferencing nil BIO function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for representative CMS memory helper paths
- change only `src/fafafa.ssl.openssl.api.cms.pas`
- preserve existing behavior when BIO helpers are available
- do not redesign CMS loading or global BIO loading

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_cms_helper_bio_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.cms.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, and CMS support on the current runtime
  - temporarily clears `BIO_new_mem_buf` / `BIO_free` and asserts representative sign/encrypt helpers do not raise
  - temporarily clears sink/output BIO dependencies and asserts representative verify/decrypt helpers do not raise
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal BIO guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.cms.pas`

**Steps:**
- Add early-return guards so sign/encrypt helpers require:
  - `BIO_new_mem_buf`
  - `BIO_free`
- Add early-return guards so verify/decrypt helpers require the BIO constructors / sinks they dereference:
  - `BIO_new_mem_buf`
  - `BIO_new`
  - `BIO_s_null`
  - `BIO_s_mem`
  - `BIO_free`
- Keep successful CMS behavior unchanged when those BIO helpers are available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cms_bio_contract && fpc -B -Fu./src -FUtmp/cms_bio_contract -FEtmp/cms_bio_contract -otmp/cms_bio_contract/test_cms_helper_bio_contract tests/test_cms_helper_bio_contract.pas && ./tmp/cms_bio_contract/test_cms_helper_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-cms-helper-bio-guard.md src/fafafa.ssl.openssl.api.cms.pas tests/test_cms_helper_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused CMS helper contract passes without raising
- helper entrypoints degrade to `False` / `nil` / empty bytes when BIO dependencies are unavailable
