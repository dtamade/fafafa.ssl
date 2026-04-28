# OpenSSL Connection Write Guard Plan

**Goal:** Make socket-style `TOpenSSLConnection.Write` preserve its existing integer-failure contract when `SSL_write` or the failure-path `SSL_get_error` helper is unavailable, instead of dereferencing a nil function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around public socket-style `Write`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve current stream-handshake and pump behavior when helpers are available
- do not redesign `Read`, handshake flow, or constructor ownership

## Task 1: RED - Reproduce the socket write helper gap

**Files:**
- Add: `tests/test_openssl_connection_write_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms the socket-style `TOpenSSLConnection` constructor on a dummy handle
  - uses a thin test subclass to force the connection into a connected state
  - verifies a stable baseline where stubbed `SSL_write` fails, stubbed `SSL_get_error` returns an error code, and `Write` returns `-1` without raising
  - temporarily clears `SSL_write`
  - temporarily restores a failing `SSL_write` stub but clears `SSL_get_error`
  - asserts every scenario must not raise and must return `-1`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal write guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add local guard checks in `DoWrite(...)` so socket-style `Write` degrades to `-1` when:
  - `SSL_write` is unavailable
  - `SSL_get_error` is unavailable on the failure path
- Preserve current behavior:
  - stream-based `Write` still uses its handshake and BIO pump loop
  - connected socket-style `Write` still delegates to `SSL_write` when helpers are available
  - helper loss does not raise `EAccessViolation`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_write_contract && fpc -B -Fu./src -FUtmp/openssl_connection_write_contract -FEtmp/openssl_connection_write_contract -otmp/openssl_connection_write_contract/test_openssl_connection_write_contract tests/test_openssl_connection_write_contract.pas && ./tmp/openssl_connection_write_contract/test_openssl_connection_write_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-write-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_write_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- socket-style `Write` no longer crashes when `SSL_write` or failure-path `SSL_get_error` is unavailable
- public `Write` safely preserves its `-1` degradation on helper loss
- full module compile remains green
