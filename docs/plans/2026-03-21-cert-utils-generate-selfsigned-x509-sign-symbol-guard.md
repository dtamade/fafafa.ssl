# Certificate Utils GenerateSelfSigned X509_sign Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSelfSigned(...)` preserve its existing controlled-exception contract when `X509_sign` is unavailable, instead of dereferencing a nil X509 signing function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for `TCertificateUtils.GenerateSelfSigned(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful self-signed generation behavior when `X509_sign` is available
- do not redesign `GenerateSigned(...)`, `EVP_sha256`, PEM export, or broader certificate generation logic

## Task 1: RED - Reproduce the self-signed signing symbol gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_x509_sign_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - temporarily clears `X509_sign`
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal self-signed signing guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(X509_sign)` guard inside `SignCertificateWithKey(...)` before any `X509_sign(...)` call
- Preserve current behavior:
  - Ed25519 signing path still prefers `X509_sign(..., nil)` when the symbol is available
  - RSA/ECDSA signing path still tries SHA-256 first and nil digest fallback second when the symbol is available
  - `GenerateSelfSigned(...)` continues to raise `ESSLCertError` on signing failure
  - Try wrappers remain non-throwing and return `False`
- Leave `EVP_sha256` for a later isolated batch if it still remains a direct gap here.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_x509_sign_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_x509_sign_symbol_contract -FEtmp/cert_utils_generate_selfsigned_x509_sign_symbol_contract -otmp/cert_utils_generate_selfsigned_x509_sign_symbol_contract/test_cert_utils_generate_selfsigned_x509_sign_symbol_contract tests/test_cert_utils_generate_selfsigned_x509_sign_symbol_contract.pas && ./tmp/cert_utils_generate_selfsigned_x509_sign_symbol_contract/test_cert_utils_generate_selfsigned_x509_sign_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-cert-utils-generate-selfsigned-x509-sign-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_x509_sign_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed signing contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` when `X509_sign` is unavailable
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` return `False` and clear outputs
