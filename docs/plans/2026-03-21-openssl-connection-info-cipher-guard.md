# OpenSSL Connection Info Cipher Guard Plan

**Goal:** Make `TOpenSSLConnection.GetConnectionInfo` preserve its inherited safe defaults when `SSL_get_current_cipher` or `SSL_CIPHER_get_name` is unavailable, instead of dereferencing nil function pointers in the OpenSSL-specific aggregation step.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GetConnectionInfo`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve inherited/default `TSSLConnectionInfo` values when cipher helpers are unavailable
- do not redesign `GetStateString`, handshake flow, or broader connection-info parsing

## Task 1: RED - Reproduce the connection-info helper gap

**Files:**
- Add: `tests/test_openssl_connection_info_cipher_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal stream-based `TOpenSSLConnection` constructor
  - captures the fresh-connection baseline from `GetConnectionInfo`
  - constructs fresh stream-based `TOpenSSLConnection` instances on `TMemoryStream`
  - temporarily clears `SSL_get_current_cipher`
  - temporarily stubs `SSL_get_current_cipher` to return a non-nil fake cipher, clears `SSL_CIPHER_get_name`, and stubs `SSL_CIPHER_get_bits` to avoid unrelated pointer crashes
  - asserts `GetConnectionInfo` must not raise
  - asserts the inherited/default contract is preserved by comparing the degraded result to the captured fresh-connection baseline for:
    - `ProtocolVersion`
    - `CipherSuite`
    - `KeySize`
    - `ServerName`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal connection-info cipher guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add local guards in `GetConnectionInfo` so OpenSSL-specific cipher aggregation runs only when:
  - `SSL_get_current_cipher` is assigned
  - `SSL_CIPHER_get_name` is assigned before reading the cipher name
- Preserve current behavior:
  - `Result := inherited GetConnectionInfo` remains the baseline
  - missing cipher helpers do not raise `EAccessViolation`
  - guarded `SSL_CIPHER_get_bits` and `SSL_get_servername` behavior stays unchanged

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_info_cipher_contract && fpc -B -Fu./src -FUtmp/openssl_connection_info_cipher_contract -FEtmp/openssl_connection_info_cipher_contract -otmp/openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-info-cipher-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_info_cipher_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- `GetConnectionInfo` no longer crashes when cipher helpers are unavailable
- the override preserves inherited/default connection info values under helper loss
- full module compile remains green
