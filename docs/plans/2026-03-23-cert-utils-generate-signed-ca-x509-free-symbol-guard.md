# Certificate Utils GenerateSigned CA X509_free Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSigned(...)` preserve its existing exception-based contract when `X509_free` becomes unavailable on the outer CA-certificate cleanup path, instead of dereferencing a nil certificate cleanup helper after leaf-certificate cleanup and private-key cleanup have already completed.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSigned(...)` and `TryGenerateSigned(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful CA-signed certificate generation behavior when `X509_free` is available
- keep this batch limited to the later CA cleanup call site:
  - `X509_free(LCACert)`
- because the earlier leaf cleanup already guards `X509_free(LCert)`, use a wrapper-based RED that:
  - installs a temporary `X509_free` wrapper
  - lets the first leaf cleanup succeed
  - disables the global `X509_free` symbol immediately after that first cleanup
  - exposes only the later CA cleanup dereference
- do not redesign `GenerateSelfSigned(...)`, `EVP_PKEY_free`, or broader cleanup ordering logic

## Task 1: RED - Reproduce the signed outer CA certificate cleanup gap

**Files:**
- Add: `tests/test_cert_utils_generate_signed_ca_x509_free_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by `GenerateSigned(...)`
  - warms a normal CA generation + `GenerateSigned(...)` path
  - installs an `X509_free` wrapper that allows the first leaf cleanup to succeed and then clears the global `X509_free` symbol before the later `X509_free(LCACert)` call site
  - asserts direct `TCertificateUtils.GenerateSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSigned(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal signed outer CA certificate cleanup guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(X509_free)` guard inside `TCertificateUtils.GenerateSigned(...)` before `X509_free(LCACert)`
- Preserve current behavior:
  - direct `GenerateSigned(...)` raises controlled `ESSLCertError` when CA certificate cleanup helpers are unavailable
  - successful generation remains unchanged when `X509_free` is available
  - `TryGenerateSigned(...)` remains non-throwing and returns `False`
  - earlier leaf `X509_free(LCert)` and `EVP_PKEY_free` cleanup helpers stay unchanged

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_signed_ca_x509_free_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_ca_x509_free_symbol_contract -FEtmp/cert_utils_generate_signed_ca_x509_free_symbol_contract -otmp/cert_utils_generate_signed_ca_x509_free_symbol_contract/test_cert_utils_generate_signed_ca_x509_free_symbol_contract tests/test_cert_utils_generate_signed_ca_x509_free_symbol_contract.pas && ./tmp/cert_utils_generate_signed_ca_x509_free_symbol_contract/test_cert_utils_generate_signed_ca_x509_free_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-23-cert-utils-generate-signed-ca-x509-free-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_signed_ca_x509_free_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused signed-generation contract passes without raw `EAccessViolation`
- direct `GenerateSigned(...)` raises `ESSLCertError` when `X509_free` is unavailable on the outer CA-certificate cleanup path
- `TryGenerateSigned(...)` returns `False` and clears outputs
