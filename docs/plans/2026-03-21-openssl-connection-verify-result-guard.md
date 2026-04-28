# OpenSSL Connection Verify Result Guard Plan

**Goal:** Make `TOpenSSLConnection.GetVerifyResult` preserve its existing `-1` degradation when `SSL_get_verify_result` is unavailable, instead of dereferencing a nil function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GetVerifyResult`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve the existing `-1` result when verification status cannot be queried
- do not redesign `GetVerifyResultString`, certificate validation, or handshake logic

## Task 1: RED - Reproduce the verify-result helper gap

**Files:**
- Add: `tests/test_openssl_connection_verify_result_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal stream-based `TOpenSSLConnection` constructor
  - constructs a fresh stream-based `TOpenSSLConnection` on `TMemoryStream`
  - temporarily clears `SSL_get_verify_result`
  - asserts `GetVerifyResult` must not raise and must return `-1`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal verify-result guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add a local `Assigned(SSL_get_verify_result)` guard in `DoGetVerifyResult(...)`
- Preserve current behavior:
  - `FSSL=nil` still returns `-1`
  - missing verify-result helper still returns `-1`
  - helper loss does not raise `EAccessViolation`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_verify_result_contract && fpc -B -Fu./src -FUtmp/openssl_connection_verify_result_contract -FEtmp/openssl_connection_verify_result_contract -otmp/openssl_connection_verify_result_contract/test_openssl_connection_verify_result_contract tests/test_openssl_connection_verify_result_contract.pas && ./tmp/openssl_connection_verify_result_contract/test_openssl_connection_verify_result_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-verify-result-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_verify_result_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- `GetVerifyResult` no longer crashes when `SSL_get_verify_result` is unavailable
- direct `GetVerifyResult` safely preserves its `-1` contract
- full module compile remains green
