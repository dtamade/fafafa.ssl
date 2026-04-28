# Certificate Utils GenerateSigned Certificate PEM Export BIO_new Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSigned(...)` preserve its existing exception-based contract when `BIO_new` becomes unavailable at the certificate PEM export constructor call site, instead of dereferencing a nil BIO allocation helper after the certificate PEM helper gate has already passed.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSigned(...)` and `TryGenerateSigned(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful CA-signed certificate generation behavior when `BIO_new` is available
- keep this batch limited to the earliest reachable remaining certificate PEM export helper:
  - `GenerateSigned(...)` -> `LBIO := BIO_new(BIO_s_mem())`
- because `HasCertificatePEMWriteBIOHelpers` checks `BIO_new` at entry, use a wrapper-based RED that:
  - keeps `BIO_new` assigned long enough to pass the helper gate
  - installs a temporary `BIO_s_mem` wrapper
  - lets the helper gate see a valid `BIO_s_mem`
  - disables the global `BIO_new` symbol immediately before the local `BIO_new(...)` dereference
  - exposes only this certificate PEM export constructor call site
- do not redesign `GenerateSelfSigned(...)`, the later `PEM_write_bio_X509` / `BIO_read` / `BIO_free` / private-key export helpers, or broader certificate generation logic

## Task 1: RED - Reproduce the signed certificate PEM export BIO constructor gap

**Files:**
- Add: `tests/test_cert_utils_generate_signed_certificate_pem_export_bio_new_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by `GenerateSigned(...)`
  - warms a normal CA generation + `GenerateSigned(...)` path
  - installs a `BIO_s_mem` wrapper that clears the global `BIO_new` symbol immediately before the certificate PEM export `BIO_new(...)` call
  - asserts direct `TCertificateUtils.GenerateSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSigned(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal signed certificate PEM export BIO constructor guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(BIO_new)` guard inside `TCertificateUtils.GenerateSigned(...)` immediately around the certificate PEM export `LBIO := BIO_new(BIO_s_mem())` constructor sequence
- Preserve current behavior:
  - direct `GenerateSigned(...)` raises controlled `ESSLCertError` when certificate PEM export constructor helpers are unavailable after the helper gate
  - successful generation remains unchanged when `BIO_new` is available
  - `TryGenerateSigned(...)` remains non-throwing and returns `False`
  - later certificate/private-key PEM export helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_signed_certificate_pem_export_bio_new_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_certificate_pem_export_bio_new_symbol_contract -FEtmp/cert_utils_generate_signed_certificate_pem_export_bio_new_symbol_contract -otmp/cert_utils_generate_signed_certificate_pem_export_bio_new_symbol_contract/test_cert_utils_generate_signed_certificate_pem_export_bio_new_symbol_contract tests/test_cert_utils_generate_signed_certificate_pem_export_bio_new_symbol_contract.pas && ./tmp/cert_utils_generate_signed_certificate_pem_export_bio_new_symbol_contract/test_cert_utils_generate_signed_certificate_pem_export_bio_new_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-generate-signed-certificate-pem-export-bio-new-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_signed_certificate_pem_export_bio_new_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused signed-generation contract passes without raw `EAccessViolation`
- direct `GenerateSigned(...)` raises `ESSLCertError` when certificate PEM export `BIO_new` becomes unavailable after the helper gate
- `TryGenerateSigned(...)` returns `False` and clears outputs
