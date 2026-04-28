# TXT_DB Helper BIO Guard Plan

**Goal:** Make `TXTDBReadFromFile(...)` and `TXTDBWriteToFile(...)` fail safely when BIO file helpers are unavailable, instead of dereferencing nil BIO function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for the missing-BIO path
- change only `src/fafafa.ssl.openssl.api.txt_db.pas`
- do not redesign TXT_DB loading or broader BIO module behavior
- preserve current behavior when BIO helpers are available

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_txt_db_helper_bio_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.txt_db.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core and TXT_DB
  - intentionally exercises `TXTDBReadFromFile(...)` / `TXTDBWriteToFile(...)` while `BIO_new_file` remains unavailable
  - asserts the helpers return `nil` / `False` instead of raising
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal BIO guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.txt_db.pas`

**Steps:**
- In `TXTDBReadFromFile(...)`:
  - return `nil` early when `BIO_new_file` or `BIO_free` is unavailable
- In `TXTDBWriteToFile(...)`:
  - return `False` early when `BIO_new_file` or `BIO_free` is unavailable
- Keep successful BIO-backed behavior unchanged.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/txt_db_bio_contract && fpc -B -Fu./src -FUtmp/txt_db_bio_contract -FEtmp/txt_db_bio_contract -otmp/txt_db_bio_contract/test_txt_db_helper_bio_contract tests/test_txt_db_helper_bio_contract.pas && ./tmp/txt_db_bio_contract/test_txt_db_helper_bio_contract`
- `git diff --check -- docs/plans/2026-03-20-txt-db-helper-bio-guard.md src/fafafa.ssl.openssl.api.txt_db.pas tests/test_txt_db_helper_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused TXT_DB helper contract passes without raising
- helpers degrade to `nil` / `False` when BIO file helpers are unavailable
