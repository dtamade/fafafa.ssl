# OpenSSL Connection Stream Cleanup SSL_free Guard Plan

**Goal:** Make public stream `CreateConnection(...)` preserve its existing controlled memory-failure contract when partial stream-constructor cleanup needs `SSL_free` but that helper is unavailable, instead of collapsing the original error into a wrapped access-violation-style failure.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around public stream `CreateConnection`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve existing successful stream-constructor and BIO setup behavior when helpers are available
- do not redesign `SSL_new`, BIO setup checks, handshake flow, or broader error mapping

## Task 1: RED - Reproduce the partial-init `SSL_free` cleanup gap

**Files:**
- Add: `tests/test_openssl_connection_stream_cleanup_ssl_free_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`
- Reference: `src/fafafa.ssl.openssl.context.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal public `CreateConnection(TMemoryStream)` call to prove the fixture is valid
  - temporarily replaces `BIO_new` with a stub that succeeds on the first call and fails on the second call
  - temporarily clears `SSL_free`
  - asserts this partial-init cleanup scenario must:
    - raise
    - raise controlled `ESSLOutOfMemoryException`
    - not raise `EAccessViolation`
    - not surface raw `Access violation` text
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal partial-init cleanup guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add a local guard in the stream constructor cleanup branch so missing `SSL_free` does not crash partial-init cleanup
- Preserve current behavior:
  - successful stream construction still attaches both BIOs with `SSL_set_bio`
  - second-BIO allocation failure still raises the original controlled memory exception
  - public `TOpenSSLContext.CreateConnection(AStream)` keeps re-raising constructor-thrown `ESSLException` values as-is

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_stream_cleanup_ssl_free_contract && fpc -B -Fu./src -FUtmp/openssl_connection_stream_cleanup_ssl_free_contract -FEtmp/openssl_connection_stream_cleanup_ssl_free_contract -otmp/openssl_connection_stream_cleanup_ssl_free_contract/test_openssl_connection_stream_cleanup_ssl_free_contract tests/test_openssl_connection_stream_cleanup_ssl_free_contract.pas && ./tmp/openssl_connection_stream_cleanup_ssl_free_contract/test_openssl_connection_stream_cleanup_ssl_free_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-stream-cleanup-ssl-free-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_stream_cleanup_ssl_free_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- public stream `CreateConnection(...)` no longer collapses partial-init cleanup under missing `SSL_free` into wrapped access-violation-style errors
- second-BIO allocation failure continues surfacing as controlled `ESSLOutOfMemoryException`
- full module compile remains green
