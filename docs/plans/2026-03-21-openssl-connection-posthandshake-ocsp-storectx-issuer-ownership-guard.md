# OpenSSL Connection Post-Handshake OCSP StoreCtx Issuer Ownership Guard Plan

**Goal:** Make `TOpenSSLConnection.ValidatePostHandshake(...)` fail closed when issuer discovery for OCSP revocation checking falls back to a temporary `X509_STORE_CTX` chain but `X509_up_ref` is unavailable, instead of carrying a borrowed issuer pointer into downstream `CheckCertificateStatus(...)` after the store context is released.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around the public `Connect` path
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve existing fail-closed connection validation behavior
- do not redesign HTTP hooks, OCSP transport, cert verify cache, or hostname policy

## Task 1: RED - Reproduce the post-handshake storectx issuer ownership gap

**Files:**
- Add: `tests/test_openssl_connection_posthandshake_ocsp_storectx_issuer_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context with `sslVerifyPeer`
  - enables `sslCertVerifyCheckOCSP` while ignoring hostname checks to keep the test scoped to OCSP ownership
  - uses a small `TOpenSSLConnection` access subclass to override `DoGetPeerCertificate`
  - creates a real minimal leaf certificate with an AIA OCSP responder URL and a real issuer certificate
  - drives the public `Connect` path by stubbing `SSL_connect` to succeed so `ValidatePostHandshake(True)` runs
  - forces issuer resolution into the `X509_STORE_CTX` fallback path by clearing `SSL_get_peer_cert_chain` and `SSL_get0_verified_chain`
  - stubs `X509_STORE_CTX_*` to return a temporary chain and mark the context released before downstream OCSP verification
  - clears `X509_up_ref`
  - supplies a fake HTTP POST hook and OCSP response stubs so `CheckCertificateStatus(...)` is reachable without real networking
  - stubs `OCSP_BASICRESP_verify` to detect whether a borrowed issuer from the released store context is passed downstream
  - asserts `Connect` must not raise, must return `False`, and must not continue with the released borrowed issuer
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal post-handshake ownership guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- In `ValidatePostHandshake(...)`, when issuer resolution succeeds only through `X509_STORE_CTX_get0_chain(...)`:
  - require `X509_up_ref` before carrying `IssuerX509` past `X509_STORE_CTX_free`
  - if `X509_up_ref` is unavailable, clear the issuer pointer so the existing fail-closed `IssuerX509=nil` path handles the failure
- Preserve current behavior:
  - no exception on helper loss
  - `Connect` / `InternalHandshake` still fail closed with `False`
  - no downstream `CheckCertificateStatus(...)` attempt with a storectx-borrowed issuer after the temporary chain is released

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_posthandshake_ocsp_storectx_issuer_contract && fpc -B -Fu./src -FUtmp/openssl_connection_posthandshake_ocsp_storectx_issuer_contract -FEtmp/openssl_connection_posthandshake_ocsp_storectx_issuer_contract -otmp/openssl_connection_posthandshake_ocsp_storectx_issuer_contract/test_openssl_connection_posthandshake_ocsp_storectx_issuer_contract tests/test_openssl_connection_posthandshake_ocsp_storectx_issuer_contract.pas && ./tmp/openssl_connection_posthandshake_ocsp_storectx_issuer_contract/test_openssl_connection_posthandshake_ocsp_storectx_issuer_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-posthandshake-ocsp-storectx-issuer-ownership-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_posthandshake_ocsp_storectx_issuer_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- `Connect` fails closed when post-handshake OCSP issuer ownership cannot be secured from the storectx fallback path
- no downstream OCSP verification is attempted with a released borrowed issuer
- full module compile remains green
