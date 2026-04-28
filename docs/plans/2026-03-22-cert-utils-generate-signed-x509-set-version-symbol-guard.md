# Certificate Utils GenerateSigned X509_set_version Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSigned(...)` preserve its existing exception-based contract when `X509_set_version` is unavailable on the leaf certificate versioning path, instead of dereferencing a nil certificate-version helper after leaf allocation has already succeeded.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSigned(...)` and `TryGenerateSigned(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful CA-signed certificate generation behavior when `X509_set_version` is available
- do not redesign `GenerateSelfSigned(...)`, leaf allocation, serial/validity helpers, name helpers, PEM export, or broader certificate generation logic

## Task 1: RED - Reproduce the signed leaf version-helper gap

**Files:**
- Add: `tests/test_cert_utils_generate_signed_x509_set_version_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by `GenerateSigned(...)`
  - warms a normal CA generation + `GenerateSigned(...)` path
  - temporarily clears `X509_set_version`
  - asserts direct `TCertificateUtils.GenerateSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSigned(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal signed leaf version-helper guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(X509_set_version)` guard inside `TCertificateUtils.GenerateSigned(...)` before `X509_set_version(LCert, X509_VERSION_3)`
- Preserve current behavior:
  - direct `GenerateSigned(...)` raises controlled `ESSLCertError` when leaf certificate version helpers are unavailable
  - successful generation remains unchanged when `X509_set_version` is available
  - `TryGenerateSigned(...)` remains non-throwing and returns `False`
  - later serial/validity/name/extension/export/cleanup helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_signed_x509_set_version_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_x509_set_version_symbol_contract -FEtmp/cert_utils_generate_signed_x509_set_version_symbol_contract -otmp/cert_utils_generate_signed_x509_set_version_symbol_contract/test_cert_utils_generate_signed_x509_set_version_symbol_contract tests/test_cert_utils_generate_signed_x509_set_version_symbol_contract.pas && ./tmp/cert_utils_generate_signed_x509_set_version_symbol_contract/test_cert_utils_generate_signed_x509_set_version_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-generate-signed-x509-set-version-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_signed_x509_set_version_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused signed-generation contract passes without raw `EAccessViolation`
- direct `GenerateSigned(...)` raises `ESSLCertError` when `X509_set_version` is unavailable on the leaf versioning path
- `TryGenerateSigned(...)` returns `False` and clears outputs
