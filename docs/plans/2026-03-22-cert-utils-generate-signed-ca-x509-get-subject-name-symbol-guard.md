# Certificate Utils GenerateSigned CA X509_get_subject_name Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSigned(...)` preserve its existing exception-based contract when the CA-side `X509_get_subject_name` helper becomes unavailable after leaf subject construction has already succeeded, instead of dereferencing a nil helper during issuer-name setup.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSigned(...)` and `TryGenerateSigned(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful CA-signed certificate generation behavior when the CA `X509_get_subject_name` call remains available
- do not redesign `GenerateSelfSigned(...)`, the existing leaf `X509_get_subject_name` guard, `AddNameEntry(...)`, `X509_set_issuer_name`, PEM export, or broader certificate generation logic

## Task 1: RED - Reproduce the CA subject-name getter gap

**Files:**
- Add: `tests/test_cert_utils_generate_signed_ca_x509_get_subject_name_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by `GenerateSigned(...)`
  - warms a normal CA generation + `GenerateSigned(...)` path
  - installs a wrapper for `X509_get_subject_name` that allows the first leaf call to succeed and then clears the global symbol before the CA call
  - asserts direct `TCertificateUtils.GenerateSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSigned(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal CA subject-name getter guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(X509_get_subject_name)` guard inside `TCertificateUtils.GenerateSigned(...)` before `LCAName := X509_get_subject_name(LCACert)`
- Preserve current behavior:
  - direct `GenerateSigned(...)` raises controlled `ESSLCertError` when CA subject-name helpers are unavailable at issuer setup time
  - successful generation remains unchanged when the CA `X509_get_subject_name` call remains available
  - `TryGenerateSigned(...)` remains non-throwing and returns `False`
  - later `X509_set_issuer_name`, extension chain, PEM export, and cleanup helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_signed_ca_x509_get_subject_name_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_ca_x509_get_subject_name_symbol_contract -FEtmp/cert_utils_generate_signed_ca_x509_get_subject_name_symbol_contract -otmp/cert_utils_generate_signed_ca_x509_get_subject_name_symbol_contract/test_cert_utils_generate_signed_ca_x509_get_subject_name_symbol_contract tests/test_cert_utils_generate_signed_ca_x509_get_subject_name_symbol_contract.pas && ./tmp/cert_utils_generate_signed_ca_x509_get_subject_name_symbol_contract/test_cert_utils_generate_signed_ca_x509_get_subject_name_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-generate-signed-ca-x509-get-subject-name-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_signed_ca_x509_get_subject_name_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused signed-generation contract passes without raw `EAccessViolation`
- direct `GenerateSigned(...)` raises `ESSLCertError` when the CA `X509_get_subject_name` helper becomes unavailable during issuer setup
- `TryGenerateSigned(...)` returns `False` and clears outputs
