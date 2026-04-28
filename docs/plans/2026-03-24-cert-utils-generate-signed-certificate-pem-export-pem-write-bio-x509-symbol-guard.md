# Certificate Utils GenerateSigned Certificate PEM Export PEM_write_bio_X509 Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSigned(...)` preserve its existing exception-based contract when `PEM_write_bio_X509` becomes unavailable at the certificate PEM export write call site, instead of dereferencing a nil PEM write helper after the certificate PEM helper gate and BIO constructor have already succeeded.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSigned(...)` and `TryGenerateSigned(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful CA-signed certificate generation behavior when `PEM_write_bio_X509` is available
- keep this batch limited to the next earliest reachable remaining certificate PEM export helper:
  - `GenerateSigned(...)` -> `PEM_write_bio_X509(LBIO, LCert)`
- because `HasCertificatePEMWriteBIOHelpers` checks `PEM_write_bio_X509` at entry, use a wrapper-based RED that:
  - keeps `PEM_write_bio_X509` assigned long enough to pass the helper gate
  - installs a temporary `BIO_new` wrapper
  - lets the certificate PEM export BIO constructor succeed
  - disables the global `PEM_write_bio_X509` symbol immediately after that successful constructor returns
  - exposes only this certificate PEM export write call site
- do not redesign `GenerateSelfSigned(...)`, the later `BIO_read` / `BIO_free` / private-key export helpers, or broader certificate generation logic

## Task 1: RED - Reproduce the signed certificate PEM export write gap

**Files:**
- Add: `tests/test_cert_utils_generate_signed_certificate_pem_export_pem_write_bio_x509_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by `GenerateSigned(...)`
  - warms a normal CA generation + `GenerateSigned(...)` path
  - installs a `BIO_new` wrapper that clears the global `PEM_write_bio_X509` symbol immediately after the certificate PEM export constructor succeeds
  - asserts direct `TCertificateUtils.GenerateSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSigned(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal signed certificate PEM export write guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(PEM_write_bio_X509)` guard inside `TCertificateUtils.GenerateSigned(...)` immediately before the certificate PEM export `PEM_write_bio_X509(LBIO, LCert)` call
- Preserve current behavior:
  - direct `GenerateSigned(...)` raises controlled `ESSLCertError` when certificate PEM export write helpers are unavailable after the helper gate and constructor
  - successful generation remains unchanged when `PEM_write_bio_X509` is available
  - `TryGenerateSigned(...)` remains non-throwing and returns `False`
  - later certificate/private-key PEM export helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_signed_certificate_pem_export_pem_write_bio_x509_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_certificate_pem_export_pem_write_bio_x509_symbol_contract -FEtmp/cert_utils_generate_signed_certificate_pem_export_pem_write_bio_x509_symbol_contract -otmp/cert_utils_generate_signed_certificate_pem_export_pem_write_bio_x509_symbol_contract/test_cert_utils_generate_signed_certificate_pem_export_pem_write_bio_x509_symbol_contract tests/test_cert_utils_generate_signed_certificate_pem_export_pem_write_bio_x509_symbol_contract.pas && ./tmp/cert_utils_generate_signed_certificate_pem_export_pem_write_bio_x509_symbol_contract/test_cert_utils_generate_signed_certificate_pem_export_pem_write_bio_x509_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-generate-signed-certificate-pem-export-pem-write-bio-x509-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_signed_certificate_pem_export_pem_write_bio_x509_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused signed-generation contract passes without raw `EAccessViolation`
- direct `GenerateSigned(...)` raises `ESSLCertError` when certificate PEM export `PEM_write_bio_X509` becomes unavailable after the helper gate
- `TryGenerateSigned(...)` returns `False` and clears outputs
