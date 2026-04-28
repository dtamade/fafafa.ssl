# Certificate Utils GenerateSelfSigned BIO_read Symbol Guard Plan

**Goal:** Make the `TCertificateUtils.GenerateSelfSigned(...)` execution path preserve its existing exception-based contract when `BIO_read` is unavailable on the certificate PEM export path, instead of dereferencing a nil BIO read helper after the certificate has already been written into the memory BIO.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSelfSigned(...)` and its Try wrappers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful self-signed generation behavior when `BIO_read` is available
- do not redesign `GenerateSigned(...)`, key PEM export, cleanup helpers, or broader certificate generation logic

## Task 1: RED - Reproduce the self-signed certificate PEM export read gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_bio_read_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by `GenerateSelfSigned(...)`
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - temporarily clears `BIO_read`
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal certificate PEM export BIO_read guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(BIO_read)` guard inside `TCertificateUtils.GenerateSelfSigned(...)` before the first certificate PEM export `BIO_read(...)` call
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when certificate PEM export read helpers are unavailable
  - successful generation remains unchanged when `BIO_read` is available
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `False`
  - later key PEM export and cleanup helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_bio_read_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_bio_read_symbol_contract -FEtmp/cert_utils_generate_selfsigned_bio_read_symbol_contract -otmp/cert_utils_generate_selfsigned_bio_read_symbol_contract/test_cert_utils_generate_selfsigned_bio_read_symbol_contract tests/test_cert_utils_generate_selfsigned_bio_read_symbol_contract.pas && ./tmp/cert_utils_generate_selfsigned_bio_read_symbol_contract/test_cert_utils_generate_selfsigned_bio_read_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-generate-selfsigned-bio-read-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_bio_read_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed generation contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` when `BIO_read` is unavailable on the certificate PEM export path
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` return `False` and clear outputs
