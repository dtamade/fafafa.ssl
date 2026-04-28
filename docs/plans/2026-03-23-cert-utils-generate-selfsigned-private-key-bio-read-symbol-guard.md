# Certificate Utils GenerateSelfSigned Private-Key BIO_read Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSelfSigned(...)` preserve its existing exception-based contract when `BIO_read` becomes unavailable during private-key PEM export, instead of dereferencing a nil PEM readback helper after certificate PEM export has already succeeded.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSelfSigned(...)` and its Try wrappers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful self-signed certificate generation behavior when `BIO_read` is available
- keep this batch limited to the private-key PEM export `BIO_read(...)`
- use a wrapper-based RED that:
  - lets the first certificate PEM export `BIO_read(...)` succeed
  - clears the global `BIO_read` symbol immediately before the second private-key PEM export readback
  - exposes only that later private-key export dereference
- do not redesign `GenerateSigned(...)`, private-key PEM export cleanup `BIO_free`, or outer cleanup logic

## Task 1: RED - Reproduce the self-signed private-key export `BIO_read` gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_private_key_bio_read_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/X509v3/EVP support required by `GenerateSelfSigned(...)`
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - installs a `BIO_read` wrapper that allows the first certificate PEM export read and then clears the global symbol before the second private-key export read
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal self-signed private-key PEM export read guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(BIO_read)` guard inside `TCertificateUtils.GenerateSelfSigned(...)` before the private-key PEM export `BIO_read(...)`
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when private-key PEM export read helpers are unavailable
  - successful generation remains unchanged when `BIO_read` is available
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `False`
  - later private-key PEM export cleanup and outer cleanup helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_private_key_bio_read_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_private_key_bio_read_symbol_contract -FEtmp/cert_utils_generate_selfsigned_private_key_bio_read_symbol_contract -otmp/cert_utils_generate_selfsigned_private_key_bio_read_symbol_contract/test_cert_utils_generate_selfsigned_private_key_bio_read_symbol_contract tests/test_cert_utils_generate_selfsigned_private_key_bio_read_symbol_contract.pas && ./tmp/cert_utils_generate_selfsigned_private_key_bio_read_symbol_contract/test_cert_utils_generate_selfsigned_private_key_bio_read_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-23-cert-utils-generate-selfsigned-private-key-bio-read-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_private_key_bio_read_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed generation contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` when private-key PEM export `BIO_read` is unavailable
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` return `False` and clear outputs
