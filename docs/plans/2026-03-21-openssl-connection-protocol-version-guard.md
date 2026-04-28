# OpenSSL Connection Protocol Version Guard Plan

**Goal:** Make `TOpenSSLConnection.GetProtocolVersion` preserve its existing safe-default contract when `SSL_version` is unavailable, instead of dereferencing a nil function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GetProtocolVersion`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve current default-return behavior when helpers are unavailable
- do not redesign handshake, cipher queries, or connection-info aggregation

## Task 1: RED - Reproduce the protocol-version helper gap

**Files:**
- Add: `tests/test_openssl_connection_protocol_version_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal stream-based `TOpenSSLConnection` constructor
  - constructs a fresh stream-based `TOpenSSLConnection` on `TMemoryStream`
  - temporarily clears `SSL_version`
  - asserts `GetProtocolVersion` must not raise and must return the existing default `sslProtocolTLS12`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal protocol-version guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add a local guard in `DoGetProtocolVersion` so `SSL_version(FSSL)` is called only when the helper is available
- Preserve current behavior:
  - `FSSL=nil` still degrades to `sslProtocolTLS12`
  - unknown protocol values still degrade to `sslProtocolTLS12`
  - helper loss does not raise `EAccessViolation`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_protocol_version_contract && fpc -B -Fu./src -FUtmp/openssl_connection_protocol_version_contract -FEtmp/openssl_connection_protocol_version_contract -otmp/openssl_connection_protocol_version_contract/test_openssl_connection_protocol_version_contract tests/test_openssl_connection_protocol_version_contract.pas && ./tmp/openssl_connection_protocol_version_contract/test_openssl_connection_protocol_version_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-protocol-version-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_protocol_version_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- `GetProtocolVersion` no longer crashes when `SSL_version` is unavailable
- direct `GetProtocolVersion` safely preserves its default `sslProtocolTLS12` contract
- full module compile remains green
