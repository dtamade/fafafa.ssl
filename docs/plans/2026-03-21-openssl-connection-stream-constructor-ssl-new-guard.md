# OpenSSL Connection Stream Constructor SSL_new Guard Plan

**Goal:** Make stream-style `TOpenSSLConnection` construction surface a precise function-not-found public exception contract when `SSL_new` is unavailable, instead of collapsing helper loss into a wrapped access-violation-style failure.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around public stream `CreateConnection`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve existing successful stream-constructor behavior when helpers are available
- do not redesign BIO setup, partial cleanup, handshake flow, or broader error mapping

## Task 1: RED - Reproduce the stream constructor `SSL_new` helper gap

**Files:**
- Add: `tests/test_openssl_connection_stream_constructor_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`
- Reference: `src/fafafa.ssl.openssl.context.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal public `CreateConnection(TMemoryStream)` call to prove the fixture is valid
  - temporarily clears `SSL_new`
  - asserts stream constructor helper loss must:
    - raise
    - raise a controlled `ESSLException`
    - carry `sslErrFunctionNotFound`
    - mention `SSL_new`
    - not raise `EAccessViolation`
    - not surface raw `Access violation` text
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal stream constructor `SSL_new` guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add a local guard check in `TOpenSSLConnection.Create(AContext, AStream)` so stream construction raises `RaiseFunctionNotAvailable('SSL_new')` when `SSL_new` is unavailable
- Preserve current behavior:
  - successful stream construction still allocates `FSSL`, creates BIOs, and attaches them with `SSL_set_bio`
  - helper-present but allocation-failed behavior still uses `RaiseSSLInitError(...)`
  - public `TOpenSSLContext.CreateConnection(AStream)` keeps re-raising constructor-thrown `ESSLException` values as-is

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_stream_constructor_contract && fpc -B -Fu./src -FUtmp/openssl_connection_stream_constructor_contract -FEtmp/openssl_connection_stream_constructor_contract -otmp/openssl_connection_stream_constructor_contract/test_openssl_connection_stream_constructor_contract tests/test_openssl_connection_stream_constructor_contract.pas && ./tmp/openssl_connection_stream_constructor_contract/test_openssl_connection_stream_constructor_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-stream-constructor-ssl-new-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_stream_constructor_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- public stream `CreateConnection(...)` no longer collapses missing `SSL_new` into wrapped access-violation-style errors
- helper loss raises a controlled `ESSLException` with `sslErrFunctionNotFound` and `SSL_new`
- full module compile remains green
