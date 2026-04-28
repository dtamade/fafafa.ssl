# OpenSSL Connection Cipher Name Guard Plan

**Goal:** Make `TOpenSSLConnection.GetCipherName` preserve its existing empty-string contract when `SSL_get_current_cipher` or `SSL_CIPHER_get_name` is unavailable, instead of dereferencing nil function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GetCipherName`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve current empty-string behavior when cipher helpers are unavailable
- do not redesign handshake, connection info aggregation, or state queries

## Task 1: RED - Reproduce the cipher-name helper gap

**Files:**
- Add: `tests/test_openssl_connection_cipher_name_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal stream-based `TOpenSSLConnection` constructor
  - constructs a fresh stream-based `TOpenSSLConnection` on `TMemoryStream`
  - temporarily clears `SSL_get_current_cipher`
  - temporarily clears `SSL_CIPHER_get_name` while stubbing `SSL_get_current_cipher` to return a non-nil cipher pointer
  - asserts `GetCipherName` must not raise and must return `''`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal cipher-name guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add local guards in `DoGetCipherName(...)` so cipher helper calls run only when available
- Preserve current behavior:
  - `FSSL=nil` still returns `''`
  - missing cipher helper(s) still returns `''`
  - helper loss does not raise `EAccessViolation`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_cipher_name_contract && fpc -B -Fu./src -FUtmp/openssl_connection_cipher_name_contract -FEtmp/openssl_connection_cipher_name_contract -otmp/openssl_connection_cipher_name_contract/test_openssl_connection_cipher_name_contract tests/test_openssl_connection_cipher_name_contract.pas && ./tmp/openssl_connection_cipher_name_contract/test_openssl_connection_cipher_name_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-cipher-name-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_cipher_name_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- `GetCipherName` no longer crashes when cipher helpers are unavailable
- direct `GetCipherName` safely preserves its empty-string contract
- full module compile remains green
