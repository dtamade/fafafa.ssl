# Certificate Utils GenerateSelfSigned Certificate PEM Export BIO_free Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSelfSigned(...)` preserve its existing exception-based contract when `BIO_free` becomes unavailable in the certificate PEM export `finally` block, instead of dereferencing a nil BIO cleanup helper after the certificate PEM readback succeeds.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSelfSigned(...)` and its Try wrappers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful self-signed certificate generation behavior when `BIO_free` is available
- keep this batch limited to the certificate PEM export cleanup `finally BIO_free(LBIO)`
- because `HasCertificatePEMWriteBIOHelpers` checks `BIO_free` at entry, use a wrapper-based RED that:
  - keeps `BIO_free` assigned long enough to pass the helper gate
  - installs a temporary `BIO_read` wrapper
  - lets the first certificate PEM export read succeed
  - disables the global `BIO_free` symbol immediately after that successful read
  - exposes only the following certificate PEM export cleanup dereference
- do not redesign `GenerateSigned(...)`, later private-key PEM export `BIO_read` / `BIO_free`, or outer cleanup logic

## Task 1: RED - Reproduce the self-signed certificate PEM export `BIO_free` gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_certificate_pem_export_bio_free_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by `GenerateSelfSigned(...)`
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - installs a `BIO_read` wrapper that allows the first certificate PEM export read to succeed and then clears the global `BIO_free` symbol before the certificate PEM export cleanup `finally`
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal self-signed certificate PEM export cleanup guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(BIO_free)` guard inside `TCertificateUtils.GenerateSelfSigned(...)` around the certificate PEM export `finally` cleanup path before `BIO_free(LBIO)`
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when certificate PEM export cleanup helpers are unavailable
  - successful generation remains unchanged when `BIO_free` is available
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `False`
  - later private-key PEM export helpers and outer cleanup helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_certificate_pem_export_bio_free_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_certificate_pem_export_bio_free_symbol_contract -FEtmp/cert_utils_generate_selfsigned_certificate_pem_export_bio_free_symbol_contract -otmp/cert_utils_generate_selfsigned_certificate_pem_export_bio_free_symbol_contract/test_cert_utils_generate_selfsigned_certificate_pem_export_bio_free_symbol_contract tests/test_cert_utils_generate_selfsigned_certificate_pem_export_bio_free_symbol_contract.pas && ./tmp/cert_utils_generate_selfsigned_certificate_pem_export_bio_free_symbol_contract/test_cert_utils_generate_selfsigned_certificate_pem_export_bio_free_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-23-cert-utils-generate-selfsigned-certificate-pem-export-bio-free-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_certificate_pem_export_bio_free_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed generation contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` when certificate PEM export `BIO_free` is unavailable
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` return `False` and clear outputs
