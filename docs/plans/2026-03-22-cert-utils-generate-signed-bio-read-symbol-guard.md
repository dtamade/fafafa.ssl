# Certificate Utils GenerateSigned BIO_read Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSigned(...)` preserve its existing exception-based contract when `BIO_read` is unavailable during certificate PEM export, instead of dereferencing a nil PEM readback helper after the full CA-signed build, extension chain, and signing steps have already succeeded.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSigned(...)` and `TryGenerateSigned(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful CA-signed certificate generation behavior when `BIO_read` is available
- do not redesign `GenerateSelfSigned(...)`, `AddExtension(...)`, signing helpers, private-key PEM export, or cleanup logic

## Task 1: RED - Reproduce the signed certificate-export `BIO_read` gap

**Files:**
- Add: `tests/test_cert_utils_generate_signed_bio_read_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by `GenerateSigned(...)`
  - warms a normal CA generation + `GenerateSigned(...)` path
  - temporarily clears `BIO_read`
  - asserts direct `TCertificateUtils.GenerateSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSigned(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal signed certificate PEM export read guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(BIO_read)` guard inside `TCertificateUtils.GenerateSigned(...)` before the first certificate PEM export `BIO_read(...)`
- Preserve current behavior:
  - direct `GenerateSigned(...)` raises controlled `ESSLCertError` when certificate PEM export read helpers are unavailable
  - successful generation remains unchanged when `BIO_read` is available
  - `TryGenerateSigned(...)` remains non-throwing and returns `False`
  - later private-key export `BIO_read` and cleanup helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_signed_bio_read_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_bio_read_symbol_contract -FEtmp/cert_utils_generate_signed_bio_read_symbol_contract -otmp/cert_utils_generate_signed_bio_read_symbol_contract/test_cert_utils_generate_signed_bio_read_symbol_contract tests/test_cert_utils_generate_signed_bio_read_symbol_contract.pas && ./tmp/cert_utils_generate_signed_bio_read_symbol_contract/test_cert_utils_generate_signed_bio_read_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-generate-signed-bio-read-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_signed_bio_read_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused signed-generation contract passes without raw `EAccessViolation`
- direct `GenerateSigned(...)` raises `ESSLCertError` when certificate PEM export `BIO_read` is unavailable
- `TryGenerateSigned(...)` returns `False` and clears outputs
