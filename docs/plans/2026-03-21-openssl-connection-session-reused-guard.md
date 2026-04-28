# OpenSSL Connection Session Reused Guard Plan

**Goal:** Make `TOpenSSLConnection.IsSessionReused` preserve its existing `False` degradation when `SSL_session_reused` is unavailable, instead of dereferencing a nil function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `IsSessionReused`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve the existing boolean-false behavior when session reuse cannot be determined
- do not redesign session acquisition, session setting, or verification logic

## Task 1: RED - Reproduce the session-reused helper gap

**Files:**
- Add: `tests/test_openssl_connection_session_reused_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal stream-based `TOpenSSLConnection` constructor
  - constructs a fresh stream-based `TOpenSSLConnection` on `TMemoryStream`
  - temporarily clears `SSL_session_reused`
  - asserts `IsSessionReused` must not raise and must return `False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal session-reused guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add a local `Assigned(SSL_session_reused)` guard in `DoIsSessionReused(...)`
- Preserve current behavior:
  - `FSSL=nil` still returns `False`
  - missing session-reused helper still returns `False`
  - helper loss does not raise `EAccessViolation`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_session_reused_contract && fpc -B -Fu./src -FUtmp/openssl_connection_session_reused_contract -FEtmp/openssl_connection_session_reused_contract -otmp/openssl_connection_session_reused_contract/test_openssl_connection_session_reused_contract tests/test_openssl_connection_session_reused_contract.pas && ./tmp/openssl_connection_session_reused_contract/test_openssl_connection_session_reused_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-session-reused-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_session_reused_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- `IsSessionReused` no longer crashes when `SSL_session_reused` is unavailable
- direct `IsSessionReused` safely preserves its `False` contract
- full module compile remains green
