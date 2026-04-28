# Encoding Base64 BIO Guard Plan

**Goal:** Make Base64 helpers in `src/fafafa.ssl.encoding.pas` fail with controlled errors when required BIO helpers are unavailable, instead of dereferencing nil BIO function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around Base64 encode/decode helpers
- change only `src/fafafa.ssl.encoding.pas`
- preserve existing successful Base64 behavior when BIO helpers are available
- do not redesign encoding initialization or the broader OpenSSL loader

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_encoding_base64_bio_contract.pas`
- Reference: `src/fafafa.ssl.encoding.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`

**Steps:**
- Write a focused contract test that:
  - warms up encoding initialization on the current runtime
  - temporarily clears representative encode/decode BIO helpers such as `BIO_push`, `BIO_free_all`, `BIO_new_mem_buf`, and `BIO_read`
  - asserts Base64 helpers must not crash with `EAccessViolation`
  - asserts the exception-based helpers raise controlled crypto errors and the `Try*` wrappers degrade to `False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal BIO guard

**Files:**
- Modify: `src/fafafa.ssl.encoding.pas`

**Steps:**
- Add narrow helper checks for the BIO functions each Base64 path dereferences:
  - encode / encode-view: `BIO_new`, `BIO_s_mem`, `BIO_f_base64`, `BIO_push`, `BIO_write`, `BIO_free_all`
  - decode: `BIO_new_mem_buf`, `BIO_new`, `BIO_f_base64`, `BIO_push`, `BIO_read`, `BIO_free_all`
- When dependencies are missing, raise a controlled crypto error instead of touching nil function pointers.
- Keep current `Try*` wrapper behavior unchanged.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/encoding_bio_contract && fpc -B -Fu./src -FUtmp/encoding_bio_contract -FEtmp/encoding_bio_contract -otmp/encoding_bio_contract/test_encoding_base64_bio_contract tests/test_encoding_base64_bio_contract.pas && ./tmp/encoding_bio_contract/test_encoding_base64_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-encoding-base64-bio-guard.md src/fafafa.ssl.encoding.pas tests/test_encoding_base64_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused Base64 BIO contract passes without `EAccessViolation`
- Base64 helpers raise controlled errors or return `False` through `Try*` wrappers when BIO dependencies are unavailable
