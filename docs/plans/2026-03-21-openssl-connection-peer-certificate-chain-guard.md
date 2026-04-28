# OpenSSL Connection Peer Certificate Chain Guard Plan

**Goal:** Make `TOpenSSLConnection.GetPeerCertificateChain` preserve its existing empty-array degradation when peer-chain helpers are unavailable, instead of dereferencing nil function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GetPeerCertificateChain`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve the existing empty-array result when peer chain information cannot be queried safely
- do not redesign certificate ownership, verified-chain lookup, or OCSP/issuer logic

## Task 1: RED - Reproduce the peer-chain helper gaps

**Files:**
- Add: `tests/test_openssl_connection_peer_certificate_chain_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - warms a normal stream-based `TOpenSSLConnection` constructor
  - constructs fresh stream-based `TOpenSSLConnection` instances on `TMemoryStream`
  - temporarily removes `SSL_get_peer_cert_chain`
  - then stubs `SSL_get_peer_cert_chain` to return a non-nil fake stack and removes `sk_X509_num`
  - then keeps the fake stack path, stubs `sk_X509_num` to return `1`, and removes `sk_X509_value`
  - then returns a real temporary `PX509` from `sk_X509_value` and removes `X509_up_ref`
  - asserts every scenario must not raise and must return an empty certificate array
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal peer-chain guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Add local capability guards in `DoGetPeerCertificateChain(...)` for:
  - `SSL_get_peer_cert_chain`
  - `sk_X509_num`
  - `sk_X509_value`
  - `X509_up_ref`
- Preserve current behavior:
  - `FSSL=nil` still returns an empty array
  - missing peer-chain helpers still return an empty array
  - helper loss does not raise `EAccessViolation`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_peer_certificate_chain_contract && fpc -B -Fu./src -FUtmp/openssl_connection_peer_certificate_chain_contract -FEtmp/openssl_connection_peer_certificate_chain_contract -otmp/openssl_connection_peer_certificate_chain_contract/test_openssl_connection_peer_certificate_chain_contract tests/test_openssl_connection_peer_certificate_chain_contract.pas && ./tmp/openssl_connection_peer_certificate_chain_contract/test_openssl_connection_peer_certificate_chain_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-peer-certificate-chain-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_peer_certificate_chain_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- `GetPeerCertificateChain` no longer crashes when any of the direct peer-chain helpers are unavailable
- direct `GetPeerCertificateChain` safely preserves its empty-array contract
- full module compile remains green
