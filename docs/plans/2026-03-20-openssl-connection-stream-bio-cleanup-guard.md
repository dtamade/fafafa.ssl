# OpenSSL Connection Stream BIO Cleanup Guard Plan

**Goal:** Make the stream-based `TOpenSSLConnection` constructor fail with a controlled exception when partial BIO initialization needs cleanup but `BIO_free` is unavailable, instead of dereferencing a nil cleanup helper.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around the stream constructor
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve current successful stream-connection initialization behavior when BIO helpers are available
- do not redesign handshake flow, pump loops, or certificate verification

## Task 1: RED - Reproduce the partial-init cleanup gap

**Files:**
- Add: `tests/test_openssl_connection_stream_bio_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`
- Reference: `src/fafafa.ssl.openssl.api.ssl.pas`

**Steps:**
- Write a focused contract test that:
  - initializes the OpenSSL library and creates a real client context
  - warms a normal stream-based `TOpenSSLConnection` constructor path before stubbing helpers
  - temporarily replaces `BIO_new` with a stub that succeeds on the first call and fails on the second call
  - temporarily clears `BIO_free`
  - asserts `TOpenSSLConnection.Create(AContext, AStream)` must not raise `EAccessViolation`
  - asserts the constructor fails with a controlled `ESSLOutOfMemoryException`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal cleanup guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add a local guard in the stream constructor’s partial-init cleanup branch so missing `BIO_free` does not crash construction
- Preserve current behavior:
  - successful stream construction remains unchanged
  - second-BIO allocation failure still raises a controlled memory exception

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_stream_bio_contract && fpc -B -Fu./src -FUtmp/openssl_connection_stream_bio_contract -FEtmp/openssl_connection_stream_bio_contract -otmp/openssl_connection_stream_bio_contract/test_openssl_connection_stream_bio_contract tests/test_openssl_connection_stream_bio_contract.pas && ./tmp/openssl_connection_stream_bio_contract/test_openssl_connection_stream_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-openssl-connection-stream-bio-cleanup-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_stream_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused stream-constructor BIO contract passes without `EAccessViolation`
- partial BIO initialization failure raises controlled memory exceptions even when `BIO_free` is unavailable
