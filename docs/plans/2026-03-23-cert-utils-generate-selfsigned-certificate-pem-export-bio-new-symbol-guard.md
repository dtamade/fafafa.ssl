# Certificate Utils GenerateSelfSigned Certificate PEM Export BIO_new Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSelfSigned(...)` preserve its existing exception-based contract when `BIO_new` becomes unavailable at the certificate PEM export constructor call site, instead of dereferencing a nil BIO allocation helper after the certificate PEM helper gate has already passed.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSelfSigned(...)` and its Try wrappers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful self-signed certificate generation behavior when `BIO_new` is available
- keep this batch limited to the earliest reachable remaining certificate PEM export helper:
  - `GenerateSelfSigned(...)` -> `LBIO := BIO_new(BIO_s_mem())`
- because `HasCertificatePEMWriteBIOHelpers` checks `BIO_new` at entry, use a wrapper-based RED that:
  - keeps `BIO_new` assigned long enough to pass the helper gate
  - installs a temporary `BIO_s_mem` wrapper
  - lets the helper gate see a valid `BIO_s_mem`
  - disables the global `BIO_new` symbol immediately before the local `BIO_new(...)` dereference
  - exposes only this certificate PEM export constructor call site
- do not redesign `GenerateSigned(...)`, the later `PEM_write_bio_X509` / `BIO_read` / `BIO_free` / private-key export helpers, or broader certificate generation logic

## Task 1: RED - Reproduce the certificate PEM export BIO constructor gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_certificate_pem_export_bio_new_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/X509v3/EVP support required by `GenerateSelfSigned(...)`
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - installs a `BIO_s_mem` wrapper that clears the global `BIO_new` symbol immediately before the certificate PEM export `BIO_new(...)` call
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal certificate PEM export BIO constructor guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(BIO_new)` guard inside `TCertificateUtils.GenerateSelfSigned(...)` immediately before the certificate PEM export `LBIO := BIO_new(BIO_s_mem())`
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when certificate PEM export constructor helpers are unavailable
  - successful generation remains unchanged when `BIO_new` is available
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `False`
  - later certificate/private-key PEM export helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_certificate_pem_export_bio_new_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_certificate_pem_export_bio_new_symbol_contract -FEtmp/cert_utils_generate_selfsigned_certificate_pem_export_bio_new_symbol_contract -otmp/cert_utils_generate_selfsigned_certificate_pem_export_bio_new_symbol_contract/test_cert_utils_generate_selfsigned_certificate_pem_export_bio_new_symbol_contract tests/test_cert_utils_generate_selfsigned_certificate_pem_export_bio_new_symbol_contract.pas && ./tmp/cert_utils_generate_selfsigned_certificate_pem_export_bio_new_symbol_contract/test_cert_utils_generate_selfsigned_certificate_pem_export_bio_new_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-23-cert-utils-generate-selfsigned-certificate-pem-export-bio-new-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_certificate_pem_export_bio_new_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed generation contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` when certificate PEM export `BIO_new` becomes unavailable after the helper gate
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` return `False` and clear outputs
