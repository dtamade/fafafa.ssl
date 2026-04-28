# Certificate Utils GenerateSelfSigned Private-Key PEM Export PEM_write_bio_PrivateKey Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSelfSigned(...)` preserve its existing exception-based contract when `PEM_write_bio_PrivateKey` becomes unavailable at the private-key PEM export write call site, instead of dereferencing a nil PEM write helper after the private-key PEM helper gate and constructor have already succeeded.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSelfSigned(...)` and its Try wrappers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful self-signed certificate generation behavior when `PEM_write_bio_PrivateKey` is available
- keep this batch limited to the next earliest reachable remaining helper on the default self-signed successful path:
  - `GenerateSelfSigned(...)` -> private-key PEM export `PEM_write_bio_PrivateKey(LBIO, LKey, nil, nil, 0, nil, nil)`
- because `HasPrivateKeyPEMWriteBIOHelpers` checks `PEM_write_bio_PrivateKey` at entry, use a wrapper-based RED that:
  - keeps `PEM_write_bio_PrivateKey` assigned long enough to pass the private-key helper gate
  - installs a temporary `BIO_new` wrapper
  - lets the certificate PEM export constructor succeed untouched
  - lets the private-key export constructor succeed
  - disables the global `PEM_write_bio_PrivateKey` symbol immediately after that second successful constructor returns
  - exposes only this private-key export write call site
- do not redesign `GenerateSigned(...)`, `BIO_read`, `BIO_free`, or broader certificate generation logic

## Task 1: RED - Reproduce the private-key PEM export write gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_private_key_pem_export_pem_write_bio_privatekey_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/X509v3/EVP support required by `GenerateSelfSigned(...)`
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - installs a `BIO_new` wrapper that clears the global `PEM_write_bio_PrivateKey` symbol immediately after the private-key PEM export constructor succeeds
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal private-key PEM export write guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(PEM_write_bio_PrivateKey)` guard inside `TCertificateUtils.GenerateSelfSigned(...)` immediately before the private-key PEM export write call
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when private-key PEM export write helpers are unavailable after the helper gate and constructor
  - successful generation remains unchanged when `PEM_write_bio_PrivateKey` is available
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `False`
  - later private-key PEM export read/cleanup helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_private_key_pem_export_pem_write_bio_privatekey_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_private_key_pem_export_pem_write_bio_privatekey_symbol_contract -FEtmp/cert_utils_generate_selfsigned_private_key_pem_export_pem_write_bio_privatekey_symbol_contract -otmp/cert_utils_generate_selfsigned_private_key_pem_export_pem_write_bio_privatekey_symbol_contract/test_cert_utils_generate_selfsigned_private_key_pem_export_pem_write_bio_privatekey_symbol_contract tests/test_cert_utils_generate_selfsigned_private_key_pem_export_pem_write_bio_privatekey_symbol_contract.pas && ./tmp/cert_utils_generate_selfsigned_private_key_pem_export_pem_write_bio_privatekey_symbol_contract/test_cert_utils_generate_selfsigned_private_key_pem_export_pem_write_bio_privatekey_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-generate-selfsigned-private-key-pem-export-pem-write-bio-privatekey-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_private_key_pem_export_pem_write_bio_privatekey_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed generation contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` when private-key PEM export `PEM_write_bio_PrivateKey` becomes unavailable after the helper gate and constructor
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` return `False` and clear outputs
