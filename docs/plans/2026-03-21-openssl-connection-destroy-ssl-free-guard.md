# OpenSSL Connection Destroy SSL_free Guard Plan

**Goal:** Make `TOpenSSLConnection.Destroy` preserve its existing no-raise cleanup contract when `SSL_free` is unavailable, instead of dereferencing a nil function pointer during connection release.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around releasing a constructed socket-style `TOpenSSLConnection`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve current successful constructor and normal release behavior when helpers are available
- do not redesign shutdown, handshake, stream transport, or broader cleanup flow

## Task 1: RED - Reproduce the destructor helper gap

**Files:**
- Add: `tests/test_openssl_connection_destroy_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal socket-style `TOpenSSLConnection` constructor
  - constructs a fresh socket-style `TOpenSSLConnection` on `THandle(0)`
  - temporarily clears `SSL_free`
  - releases the connection object
  - asserts destruction must not raise
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal destructor guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add a local guard in `TOpenSSLConnection.Destroy` so `SSL_free(FSSL)` is called only when the helper is available
- Preserve current behavior:
  - connected instances still attempt `DoShutdown`
  - `FSSL` is still cleared before inherited cleanup returns
  - helper-present destruction keeps freeing the SSL handle

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_destroy_contract && fpc -B -Fu./src -FUtmp/openssl_connection_destroy_contract -FEtmp/openssl_connection_destroy_contract -otmp/openssl_connection_destroy_contract/test_openssl_connection_destroy_contract tests/test_openssl_connection_destroy_contract.pas && ./tmp/openssl_connection_destroy_contract/test_openssl_connection_destroy_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-destroy-ssl-free-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_destroy_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- destroying a constructed `TOpenSSLConnection` no longer crashes when `SSL_free` is unavailable
- connection release preserves its existing no-raise cleanup contract
- full module compile remains green
