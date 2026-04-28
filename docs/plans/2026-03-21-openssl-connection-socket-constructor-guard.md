# OpenSSL Connection Socket Constructor Guard Plan

**Goal:** Make socket-style `TOpenSSLConnection` construction surface a precise function-not-found public exception contract when `SSL_new` or `SSL_set_fd` is unavailable, instead of collapsing helper loss into a wrapped access-violation-style failure.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around public socket `CreateConnection`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve existing socket-constructor behavior when helpers are available
- do not redesign stream construction, handshake flow, or broader error mapping

## Task 1: RED - Reproduce the socket constructor helper gap

**Files:**
- Add: `tests/test_openssl_connection_socket_constructor_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`
- Reference: `src/fafafa.ssl.openssl.context.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal public `CreateConnection(THandle(0))` call to prove the fixture is valid
  - temporarily clears `SSL_new`
  - temporarily restores `SSL_new` and clears `SSL_set_fd`
  - asserts every unavailable-helper scenario must:
    - raise
    - raise a controlled `ESSLException`
    - carry `sslErrFunctionNotFound`
    - mention the missing helper name
    - not raise `EAccessViolation`
    - not surface raw `Access violation` text
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal socket constructor guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add local guard checks in `TOpenSSLConnection.Create(AContext, ASocket)` so socket construction raises a controlled SSL exception when:
  - `SSL_new` is unavailable
  - `SSL_set_fd` is unavailable
- Preserve current behavior:
  - `SSL_new(Ctx)` still drives normal allocation when the helper exists
  - socket construction still binds the socket through `SSL_set_fd` when available
  - public `TOpenSSLContext.CreateConnection(...)` keeps re-raising constructor-thrown `ESSLException` values as-is

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_socket_constructor_contract && fpc -B -Fu./src -FUtmp/openssl_connection_socket_constructor_contract -FEtmp/openssl_connection_socket_constructor_contract -otmp/openssl_connection_socket_constructor_contract/test_openssl_connection_socket_constructor_contract tests/test_openssl_connection_socket_constructor_contract.pas && ./tmp/openssl_connection_socket_constructor_contract/test_openssl_connection_socket_constructor_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-socket-constructor-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_socket_constructor_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- public socket `CreateConnection(...)` no longer collapses `SSL_new` or `SSL_set_fd` helper loss into wrapped access-violation-style errors
- helper loss raises a controlled `ESSLException` with `sslErrFunctionNotFound` and the missing helper name
- full module compile remains green
