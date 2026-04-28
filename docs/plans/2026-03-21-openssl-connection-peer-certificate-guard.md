# OpenSSL Connection Peer Certificate Guard Plan

**Goal:** Make `TOpenSSLConnection.GetPeerCertificate` preserve its existing nil-return contract when `SSL_get_peer_certificate` is unavailable, instead of dereferencing a nil function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GetPeerCertificate`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve the existing `nil` result when no peer certificate is available
- do not redesign peer-chain retrieval, certificate parsing, or session logic

## Task 1: RED - Reproduce the peer-certificate helper gap

**Files:**
- Add: `tests/test_openssl_connection_peer_certificate_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal stream-based `TOpenSSLConnection` constructor
  - constructs a fresh stream-based `TOpenSSLConnection` on `TMemoryStream`
  - temporarily clears `SSL_get_peer_certificate`
  - asserts `GetPeerCertificate` must not raise and must return `nil`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal peer-certificate guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add a local `Assigned(SSL_get_peer_certificate)` guard in `DoGetPeerCertificate(...)`
- Preserve current behavior:
  - `FSSL=nil` still returns `nil`
  - missing peer-certificate helper still returns `nil`
  - helper loss does not raise `EAccessViolation`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_peer_certificate_contract && fpc -B -Fu./src -FUtmp/openssl_connection_peer_certificate_contract -FEtmp/openssl_connection_peer_certificate_contract -otmp/openssl_connection_peer_certificate_contract/test_openssl_connection_peer_certificate_contract tests/test_openssl_connection_peer_certificate_contract.pas && ./tmp/openssl_connection_peer_certificate_contract/test_openssl_connection_peer_certificate_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-peer-certificate-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_peer_certificate_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- `GetPeerCertificate` no longer crashes when `SSL_get_peer_certificate` is unavailable
- direct `GetPeerCertificate` safely preserves its `nil` contract
- full module compile remains green
