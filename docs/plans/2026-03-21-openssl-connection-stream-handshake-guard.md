# OpenSSL Connection Stream Handshake Guard Plan

**Goal:** Make stream-based `TOpenSSLConnection.Connect` fail according to its existing boolean-return contract when required handshake helpers are unavailable, instead of dereferencing nil function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around stream-based `Connect`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve current stream-constructor and non-stream handshake behavior when helpers are available
- do not redesign read/write pump loops, OCSP logic, or constructor ownership

## Task 1: RED - Reproduce the handshake helper gap

**Files:**
- Add: `tests/test_openssl_connection_stream_handshake_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal stream-based `TOpenSSLConnection` constructor
  - uses an empty `TMemoryStream` transport so `Connect` takes the internal stream-handshake path
  - verifies a stable baseline where `Connect` returns `False` without raising when a stubbed handshake reports `WANT_READ`
  - temporarily clears:
    - `SSL_do_handshake`
    - `SSL_get_error` (while `SSL_do_handshake` is stubbed to return a non-success code)
  - asserts `Connect` must not raise and must return `False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal handshake guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add local guard checks in `InternalHandshake(...)` so missing `SSL_do_handshake` / `SSL_get_error` degrade to `False`
- Preserve current behavior:
  - stream-based handshake remains blocking and loop-driven when helpers are available
  - helper loss does not raise `EAccessViolation`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_stream_handshake_contract && fpc -B -Fu./src -FUtmp/openssl_connection_stream_handshake_contract -FEtmp/openssl_connection_stream_handshake_contract -otmp/openssl_connection_stream_handshake_contract/test_openssl_connection_stream_handshake_contract tests/test_openssl_connection_stream_handshake_contract.pas && ./tmp/openssl_connection_stream_handshake_contract/test_openssl_connection_stream_handshake_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-stream-handshake-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_stream_handshake_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- stream-based `Connect` no longer crashes when handshake helpers are unavailable
- direct `Connect` safely degrades to `False`
- full module compile remains green
