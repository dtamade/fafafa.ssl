# OpenSSL Connection OCSP StoreCtx Issuer Ownership Guard Plan

**Goal:** Make `TOpenSSLConnection.DoIsOCSPResponseVerified` fail closed when issuer discovery falls back to a temporary `X509_STORE_CTX` chain but `X509_up_ref` is unavailable, instead of carrying a borrowed issuer pointer into downstream OCSP verification after the store context is released.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `IsOCSPResponseVerified`
- change only `src/fafafa.ssl.openssl.connection.pas`
- preserve fail-closed boolean behavior
- do not redesign OCSP parsing, response verification, or post-handshake certificate policy

## Task 1: RED - Reproduce the storectx issuer ownership gap

**Files:**
- Add: `tests/test_openssl_connection_ocsp_storectx_issuer_contract.pas`
- Reference: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and creates a real client context
  - uses a small `TOpenSSLConnection` access subclass to override `DoGetOCSPResponse` and `DoGetPeerCertificate`
  - creates real minimal `PX509` leaf/issuer certificates and a real stack containing the issuer
  - forces `DoIsOCSPResponseVerified(...)` into the `X509_STORE_CTX` fallback path by clearing `SSL_get_peer_cert_chain` and `SSL_get0_verified_chain`
  - stubs `X509_STORE_CTX_*` to return a temporary chain and mark the store context as released before downstream OCSP verification
  - clears `X509_up_ref`
  - stubs `OCSP_BASICRESP_verify` to detect whether a borrowed issuer from the released store context is passed downstream
  - asserts `IsOCSPResponseVerified` must not raise, must return `False`, and must not continue with the released borrowed issuer
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal fail-closed ownership guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.connection.pas`

**Steps:**
- In `DoIsOCSPResponseVerified(...)`, when issuer resolution succeeds only through `X509_STORE_CTX_get0_chain(...)`:
  - require `X509_up_ref` before carrying `IssuerX509` past `X509_STORE_CTX_free`
  - if `X509_up_ref` is unavailable, fail closed with `False`
- Preserve current behavior:
  - no exception on helper loss
  - boolean API still degrades to `False`
  - no OCSP verification attempt with a storectx-borrowed issuer after the temporary chain is released

## Task 3: Verification

**Run:**
- `mkdir -p tmp/openssl_connection_ocsp_storectx_issuer_contract && fpc -B -Fu./src -FUtmp/openssl_connection_ocsp_storectx_issuer_contract -FEtmp/openssl_connection_ocsp_storectx_issuer_contract -otmp/openssl_connection_ocsp_storectx_issuer_contract/test_openssl_connection_ocsp_storectx_issuer_contract tests/test_openssl_connection_ocsp_storectx_issuer_contract.pas && ./tmp/openssl_connection_ocsp_storectx_issuer_contract/test_openssl_connection_ocsp_storectx_issuer_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-openssl-connection-ocsp-storectx-issuer-ownership-guard.md src/fafafa.ssl.openssl.connection.pas tests/test_openssl_connection_ocsp_storectx_issuer_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- `IsOCSPResponseVerified` fails closed when storectx-fallback issuer ownership cannot be secured
- no downstream OCSP verification is attempted with a released borrowed issuer
- full module compile remains green
