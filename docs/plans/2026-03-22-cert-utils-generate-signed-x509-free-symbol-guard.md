# Certificate Utils GenerateSigned X509_free Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSigned(...)` preserve its existing exception-based contract when `X509_free` becomes unavailable on the outer leaf-certificate cleanup path, instead of dereferencing a nil certificate cleanup helper after CA-signed certificate generation and PEM export have already completed.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSigned(...)` and `TryGenerateSigned(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful CA-signed certificate generation behavior when `X509_free` is available
- do not redesign `GenerateSelfSigned(...)`, PEM export helpers, or later `EVP_PKEY_free` / `X509_free(LCACert)` cleanup logic

## Task 1: RED - Reproduce the signed outer certificate cleanup gap

**Files:**
- Add: `tests/test_cert_utils_generate_signed_x509_free_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by `GenerateSigned(...)`
  - warms a normal CA generation + `GenerateSigned(...)` path
  - temporarily clears `X509_free` before the outer leaf cleanup
  - installs an `EVP_PKEY_free` wrapper that restores the original `X509_free` during unwind before the later `X509_free(LCACert)` cleanup
  - asserts direct `TCertificateUtils.GenerateSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSigned(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal signed outer certificate cleanup guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(X509_free)` guard inside `TCertificateUtils.GenerateSigned(...)` around the outer `finally` cleanup path before `X509_free(LCert)`
- Preserve current behavior:
  - direct `GenerateSigned(...)` raises controlled `ESSLCertError` when leaf certificate cleanup helpers are unavailable
  - successful generation remains unchanged when `X509_free` is available
  - `TryGenerateSigned(...)` remains non-throwing and returns `False`
  - later `EVP_PKEY_free(LKey)` / `EVP_PKEY_free(LCAKey)` / `X509_free(LCACert)` cleanup helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_signed_x509_free_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_x509_free_symbol_contract -FEtmp/cert_utils_generate_signed_x509_free_symbol_contract -otmp/cert_utils_generate_signed_x509_free_symbol_contract/test_cert_utils_generate_signed_x509_free_symbol_contract tests/test_cert_utils_generate_signed_x509_free_symbol_contract.pas && ./tmp/cert_utils_generate_signed_x509_free_symbol_contract/test_cert_utils_generate_signed_x509_free_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-generate-signed-x509-free-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_signed_x509_free_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused signed-generation contract passes without raw `EAccessViolation`
- direct `GenerateSigned(...)` raises `ESSLCertError` when `X509_free` is unavailable on the outer leaf-certificate cleanup path
- `TryGenerateSigned(...)` returns `False` and clears outputs
