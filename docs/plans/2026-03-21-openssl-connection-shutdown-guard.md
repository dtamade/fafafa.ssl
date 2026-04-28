# OpenSSL Connection Shutdown Guard Plan

**Goal:** Make `TOpenSSLConnection.Shutdown` preserve its existing boolean-return contract when `SSL_shutdown` is unavailable, instead of dereferencing a nil function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around stream-based `Shutdown`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve current `Shutdown` / `Close` behavior when helpers are available
- do not redesign handshake, stream pump, renegotiation, or disconnect flow

## Task 1: RED - Reproduce the shutdown helper gap

**Files:**
- Add: `tests/test_openssl_connection_shutdown_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal stream-based `TOpenSSLConnection` constructor
  - constructs a fresh stream-based `TOpenSSLConnection` on `TMemoryStream`
  - temporarily clears `SSL_shutdown`
  - asserts `Shutdown` must not raise and must return `True`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal shutdown guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add a local guard in `DoShutdown` so `SSL_shutdown(FSSL)` is called only when the helper is available
- Preserve current behavior:
  - `Shutdown` still returns `True`
  - connection state is still cleared
  - `Close` continues to delegate through `DoShutdown`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_shutdown_contract && fpc -B -Fu./src -FUtmp/openssl_connection_shutdown_contract -FEtmp/openssl_connection_shutdown_contract -otmp/openssl_connection_shutdown_contract/test_openssl_connection_shutdown_contract tests/test_openssl_connection_shutdown_contract.pas && ./tmp/openssl_connection_shutdown_contract/test_openssl_connection_shutdown_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-shutdown-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_shutdown_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- `Shutdown` no longer crashes when `SSL_shutdown` is unavailable
- direct `Shutdown` safely preserves its `True` return contract
- full module compile remains green
