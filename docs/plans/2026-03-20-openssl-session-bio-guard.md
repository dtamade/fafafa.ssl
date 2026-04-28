# OpenSSL Session BIO Guard Plan

**Goal:** Make `TOpenSSLSession.Serialize` and `TOpenSSLSession.Deserialize` fail safely when required session/BIO helpers are unavailable, instead of dereferencing nil function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `TOpenSSLSession`
- change only `src/fafafa.ssl.openssl.session.pas`
- preserve current success behavior when session/BIO helpers are available
- do not redesign OpenSSL session management or loader behavior

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_openssl_session_bio_contract.pas`
- Reference: `src/fafafa.ssl.openssl.session.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`
- Reference: `src/fafafa.ssl.openssl.api.core.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core and BIO support on the current runtime
  - creates a minimal `PSSL_SESSION` via `SSL_SESSION_new`
  - temporarily clears representative session/BIO helpers such as:
    - `i2d_SSL_SESSION_bio`
    - `BIO_s_mem`
    - `BIO_free`
    - `d2i_SSL_SESSION_bio`
    - `BIO_new_mem_buf`
  - asserts `Serialize` must not raise and must degrade to empty bytes
  - asserts `Deserialize` must not raise and must degrade to `False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal session/BIO guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.session.pas`

**Steps:**
- Add local guard checks so `Serialize` requires:
  - `i2d_SSL_SESSION_bio`
  - `BIO_new`
  - `BIO_s_mem`
  - `BIO_free`
- Add local guard checks so `Deserialize` requires:
  - `d2i_SSL_SESSION_bio`
  - `BIO_new_mem_buf`
  - `BIO_free`
- Keep current non-raising helper behavior unchanged.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_session_bio_contract && fpc -B -Fu./src -FUtmp/openssl_session_bio_contract -FEtmp/openssl_session_bio_contract -otmp/openssl_session_bio_contract/test_openssl_session_bio_contract tests/test_openssl_session_bio_contract.pas && ./tmp/openssl_session_bio_contract/test_openssl_session_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-openssl-session-bio-guard.md src/fafafa.ssl.openssl.session.pas tests/test_openssl_session_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused session helper contract passes without raising
- `Serialize` degrades to empty bytes and `Deserialize` degrades to `False` when helpers are unavailable
