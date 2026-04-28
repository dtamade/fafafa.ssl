# Certificate Utils GenerateSelfSigned AddExtension X509_EXTENSION_free Symbol Guard Plan

**Goal:** Make the `TCertificateUtils.GenerateSelfSigned(...)` execution path preserve its existing exception-based contract when `X509_EXTENSION_free` is unavailable inside `AddExtension(...)`, instead of dereferencing a nil extension-cleanup helper after the extension has already been attached.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSelfSigned(...)` and its Try wrappers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful self-signed generation behavior when `X509_EXTENSION_free` is available
- do not redesign `GenerateSigned(...)`, signing, PEM export, or broader certificate generation logic

## Task 1: RED - Reproduce the self-signed extension-cleanup gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_addextension_x509_extension_free_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads X509v3 support required by `AddExtension(...)`
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - temporarily clears `X509_EXTENSION_free`
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal AddExtension extension-cleanup guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(X509_EXTENSION_free)` guard inside `TCertificateUtils.AddExtension(...)` before `X509_EXTENSION_free(...)`
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when extension-cleanup helpers are unavailable
  - successful generation remains unchanged when `X509_EXTENSION_free` is available
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `False`
  - later signing and export helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_addextension_x509_extension_free_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_addextension_x509_extension_free_symbol_contract -FEtmp/cert_utils_generate_selfsigned_addextension_x509_extension_free_symbol_contract -otmp/cert_utils_generate_selfsigned_addextension_x509_extension_free_symbol_contract/test_cert_utils_generate_selfsigned_addextension_x509_extension_free_symbol_contract tests/test_cert_utils_generate_selfsigned_addextension_x509_extension_free_symbol_contract.pas && ./tmp/cert_utils_generate_selfsigned_addextension_x509_extension_free_symbol_contract/test_cert_utils_generate_selfsigned_addextension_x509_extension_free_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-generate-selfsigned-addextension-x509-extension-free-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_addextension_x509_extension_free_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed generation contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` when `X509_EXTENSION_free` is unavailable
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` return `False` and clear outputs
