# OpenSSL Connection Renegotiate Handshake Guard Plan

**Goal:** Make `TOpenSSLConnection.Renegotiate` fail according to its existing boolean-return contract when `SSL_do_handshake` is unavailable after `SSL_renegotiate` succeeds, instead of dereferencing a nil function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `Renegotiate`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve current `Renegotiate` behavior when helpers are available
- do not redesign stream pump, full handshake flow, or shutdown handling

## Task 1: RED - Reproduce the renegotiate handshake helper gap

**Files:**
- Add: `tests/test_openssl_connection_renegotiate_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal stream-based `TOpenSSLConnection` constructor
  - uses a thin test subclass to force the connection into a connected state
  - stubs `SSL_renegotiate` to return success
  - verifies a stable baseline where stubbed `SSL_do_handshake` returns success and `Renegotiate` returns `True`
  - then temporarily clears `SSL_do_handshake`
  - asserts `Renegotiate` must not raise and must return `False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal renegotiate handshake guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add a local guard in `DoRenegotiate` so `SSL_do_handshake(FSSL)` is called only when the helper is available
- Preserve current behavior:
  - missing `SSL_renegotiate` still degrades to `False`
  - successful renegotiation with all helpers still returns `True`
  - helper loss after renegotiation entry does not raise `EAccessViolation`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_renegotiate_contract && fpc -B -Fu./src -FUtmp/openssl_connection_renegotiate_contract -FEtmp/openssl_connection_renegotiate_contract -otmp/openssl_connection_renegotiate_contract/test_openssl_connection_renegotiate_contract tests/test_openssl_connection_renegotiate_contract.pas && ./tmp/openssl_connection_renegotiate_contract/test_openssl_connection_renegotiate_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-renegotiate-handshake-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_renegotiate_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- `Renegotiate` no longer crashes when `SSL_do_handshake` is unavailable after `SSL_renegotiate`
- direct `Renegotiate` safely degrades to `False`
- full module compile remains green
