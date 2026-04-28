# Certificate Utils GenerateSelfSigned Private-Key BIO_new Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSelfSigned(...)` preserve its existing exception-based contract when `BIO_new` becomes unavailable at the private-key PEM export constructor call site, instead of dereferencing a nil BIO allocation helper after the private-key PEM helper gate has already passed.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSelfSigned(...)` and its Try wrappers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful self-signed certificate generation behavior when `BIO_new` is available
- keep this batch limited to the next earliest reachable remaining helper on the default self-signed successful path:
  - `GenerateSelfSigned(...)` -> private-key PEM export `LBIO := BIO_new(BIO_s_mem())`
- because `HasPrivateKeyPEMWriteBIOHelpers` checks `BIO_new` at entry, use a wrapper-based RED that:
  - keeps `BIO_new` assigned long enough to pass the private-key helper gate
  - installs a temporary `BIO_s_mem` wrapper
  - lets the certificate PEM export constructor finish untouched
  - disables the global `BIO_new` symbol immediately before the second `BIO_s_mem()` return that feeds the private-key export constructor
  - exposes only this private-key export constructor call site
- do not redesign `GenerateSigned(...)`, `PEM_write_bio_PrivateKey`, `BIO_read`, `BIO_free`, or broader certificate generation logic

## Task 1: RED - Reproduce the private-key PEM export constructor gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_private_key_bio_new_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/X509v3/EVP support required by `GenerateSelfSigned(...)`
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - installs a `BIO_s_mem` wrapper that clears the global `BIO_new` symbol immediately before the private-key PEM export constructor call
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal private-key PEM export constructor guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(BIO_new)` guard inside `TCertificateUtils.GenerateSelfSigned(...)` immediately around the private-key PEM export `LBIO := BIO_new(BIO_s_mem())` constructor sequence
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when private-key PEM export constructor helpers are unavailable after the helper gate
  - successful generation remains unchanged when `BIO_new` is available
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `False`
  - later private-key PEM export write/read/cleanup helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_private_key_bio_new_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_private_key_bio_new_symbol_contract -FEtmp/cert_utils_generate_selfsigned_private_key_bio_new_symbol_contract -otmp/cert_utils_generate_selfsigned_private_key_bio_new_symbol_contract/test_cert_utils_generate_selfsigned_private_key_bio_new_symbol_contract tests/test_cert_utils_generate_selfsigned_private_key_bio_new_symbol_contract.pas && ./tmp/cert_utils_generate_selfsigned_private_key_bio_new_symbol_contract/test_cert_utils_generate_selfsigned_private_key_bio_new_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-generate-selfsigned-private-key-bio-new-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_private_key_bio_new_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed generation contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` when private-key PEM export `BIO_new` becomes unavailable after the helper gate
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` return `False` and clear outputs
