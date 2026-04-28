# Certificate Utils GenerateSigned CA Private-Key PEM_read_bio_PrivateKey Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSigned(...)` preserve its existing exception-based contract when `PEM_read_bio_PrivateKey` becomes unavailable on the CA private-key load path, instead of dereferencing a nil private-key parse helper after the CA private-key read-helper gate and BIO constructor have already succeeded.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSigned(...)` and `TryGenerateSigned(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful CA-signed certificate generation behavior when `PEM_read_bio_PrivateKey` is available
- keep this batch limited to the CA private-key load parse call:
  - `GenerateSigned(...)` -> `LCAKey := PEM_read_bio_PrivateKey(LBIO, nil, nil, nil)`
- because `HasPrivateKeyPEMReadBIOHelpers` checks `PEM_read_bio_PrivateKey` at entry, use a wrapper-based RED that:
  - keeps `PEM_read_bio_PrivateKey` assigned long enough to pass the read-helper gate
  - installs a temporary `BIO_new_mem_buf` wrapper
  - lets the CA private-key load constructor succeed
  - disables the global `PEM_read_bio_PrivateKey` symbol immediately after that successful constructor returns
  - exposes only this CA private-key parse call site
- do not redesign `GenerateSelfSigned(...)`, CA certificate load, later cleanup, leaf generation, PEM export, or outer cleanup logic

## Task 1: RED - Reproduce the signed CA private-key parse gap

**Files:**
- Add: `tests/test_cert_utils_generate_signed_ca_private_key_pem_read_bio_privatekey_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by `GenerateSigned(...)`
  - warms a normal CA generation + `GenerateSigned(...)` path
  - installs a `BIO_new_mem_buf` wrapper that clears the global `PEM_read_bio_PrivateKey` symbol immediately after the CA private-key load constructor succeeds
  - asserts direct `TCertificateUtils.GenerateSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSigned(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal signed CA private-key parse guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(PEM_read_bio_PrivateKey)` guard inside `TCertificateUtils.GenerateSigned(...)` immediately before `PEM_read_bio_PrivateKey(LBIO, nil, nil, nil)`
- Preserve current behavior:
  - direct `GenerateSigned(...)` raises controlled `ESSLCertError` when CA private-key parse helpers are unavailable after the helper gate and constructor
  - successful generation remains unchanged when `PEM_read_bio_PrivateKey` is available
  - `TryGenerateSigned(...)` remains non-throwing and returns `False`
  - later cleanup, leaf generation, and PEM export helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_signed_ca_private_key_pem_read_bio_privatekey_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_ca_private_key_pem_read_bio_privatekey_symbol_contract -FEtmp/cert_utils_generate_signed_ca_private_key_pem_read_bio_privatekey_symbol_contract -otmp/cert_utils_generate_signed_ca_private_key_pem_read_bio_privatekey_symbol_contract/test_cert_utils_generate_signed_ca_private_key_pem_read_bio_privatekey_symbol_contract tests/test_cert_utils_generate_signed_ca_private_key_pem_read_bio_privatekey_symbol_contract.pas && ./tmp/cert_utils_generate_signed_ca_private_key_pem_read_bio_privatekey_symbol_contract/test_cert_utils_generate_signed_ca_private_key_pem_read_bio_privatekey_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-generate-signed-ca-private-key-pem-read-bio-privatekey-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_signed_ca_private_key_pem_read_bio_privatekey_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused signed-generation contract passes without raw `EAccessViolation`
- direct `GenerateSigned(...)` raises `ESSLCertError` when `PEM_read_bio_PrivateKey` becomes unavailable on the CA private-key load path after the helper gate and constructor succeed
- `TryGenerateSigned(...)` returns `False` and clears outputs
