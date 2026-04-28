# OpenSSL Connection Accept Guard Plan

**Goal:** Make socket-style `TOpenSSLConnection.Accept` preserve its existing boolean-failure contract when `SSL_accept` or the failure-path `SSL_get_error` helper is unavailable, instead of dereferencing a nil function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around public socket-style `Accept`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve current post-handshake validation behavior when helpers are available
- do not redesign socket construction, stream handshake, server-side OCSP policy, or error mapping semantics beyond the local nil-call guard

## Task 1: RED - Reproduce the socket accept helper gap

**Files:**
- Add: `tests/test_openssl_connection_accept_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real server context
  - warms the socket-style `TOpenSSLConnection` constructor on a dummy handle
  - verifies a stable baseline where stubbed `SSL_accept` fails, stubbed `SSL_get_error` returns an error code, and `Accept` returns `False` without raising
  - temporarily clears `SSL_accept`
  - temporarily restores a failing `SSL_accept` stub but clears `SSL_get_error`
  - asserts every scenario must not raise and must return `False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal accept guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add local guard checks in `DoAccept(...)` so socket-style `Accept` degrades to `False` when:
  - `SSL_accept` is unavailable
  - `SSL_get_error` is unavailable on the failure path
- Preserve current behavior:
  - stream-based `Accept` still delegates to `InternalHandshake`
  - successful socket handshakes still run `ValidatePostHandshake(False)`
  - failed socket handshakes still report `False` without raising

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_accept_contract && fpc -B -Fu./src -FUtmp/openssl_connection_accept_contract -FEtmp/openssl_connection_accept_contract -otmp/openssl_connection_accept_contract/test_openssl_connection_accept_contract tests/test_openssl_connection_accept_contract.pas && ./tmp/openssl_connection_accept_contract/test_openssl_connection_accept_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-accept-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_accept_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- socket-style `Accept` no longer crashes when `SSL_accept` or failure-path `SSL_get_error` is unavailable
- public `Accept` safely preserves its `False` degradation on helper loss
- full module compile remains green
