# PEM Helper BIO Guard Plan

**Goal:** Make PEM helper entrypoints fail safely when required BIO constructors / cleanup helpers are unavailable, instead of dereferencing nil BIO function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for representative PEM file/memory helper paths
- change only `src/fafafa.ssl.openssl.api.pem.pas`
- preserve existing behavior when BIO helpers are available
- do not redesign PEM loading or global BIO loading

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_pem_helper_bio_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.pem.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`

**Steps:**
- Write a focused contract test that:
  - loads the PEM module on the current runtime
  - temporarily clears `BIO_new_file` / `BIO_free` and asserts representative file helpers do not raise
  - temporarily clears `BIO_new_mem_buf` / `BIO_free` and asserts representative memory helpers do not raise
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal BIO guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.pem.pas`

**Steps:**
- Add early-return guards so PEM file helpers require:
  - `BIO_new_file`
  - `BIO_free`
- Add early-return guards so PEM memory helpers require:
  - `BIO_new_mem_buf`
  - `BIO_free`
- Keep successful PEM parsing/writing behavior unchanged when those BIO helpers are available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/pem_bio_contract && fpc -B -Fu./src -FUtmp/pem_bio_contract -FEtmp/pem_bio_contract -otmp/pem_bio_contract/test_pem_helper_bio_contract tests/test_pem_helper_bio_contract.pas && ./tmp/pem_bio_contract/test_pem_helper_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-pem-helper-bio-guard.md src/fafafa.ssl.openssl.api.pem.pas tests/test_pem_helper_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused PEM helper contract passes without raising
- helper entrypoints degrade to `nil` / `False` when BIO dependencies are unavailable
