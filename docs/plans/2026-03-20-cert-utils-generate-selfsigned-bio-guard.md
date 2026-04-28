# Certificate Utils GenerateSelfSigned BIO Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSelfSigned(...)` fail according to its existing exception-based contract when required PEM export helpers are unavailable, instead of dereferencing nil function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSelfSigned(...)` and its Try wrappers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful self-signed generation behavior when helpers are available
- do not redesign `GenerateSigned(...)`, `VerifyChain(...)`, or broader certificate generation logic

## Task 1: RED - Reproduce the export helper gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_bio_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`
- Reference: `src/fafafa.ssl.openssl.api.pem.pas`

**Steps:**
- Write a focused contract test that:
  - initializes the OpenSSL library and loads `Core/BIO/X509/PEM/EVP`
  - warms a normal `GenerateSelfSigned(...)` path
  - temporarily clears representative export helpers:
    - `BIO_new`
    - `BIO_s_mem`
    - `BIO_free`
    - `PEM_write_bio_X509`
    - `PEM_write_bio_PrivateKey`
  - asserts direct `GenerateSelfSigned(...)` must raise a controlled certificate exception instead of `EAccessViolation`
  - asserts `TryGenerateSelfSigned(...)` / `TryGenerateSelfSignedSimple(...)` do not raise and return `False` with cleared outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal self-signed export guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add local guard checks so the self-signed certificate export path requires:
  - `BIO_new`
  - `BIO_s_mem`
  - `PEM_write_bio_X509`
  - `BIO_free`
- Add local guard checks so the self-signed private-key export path requires:
  - `BIO_new`
  - `BIO_s_mem`
  - `PEM_write_bio_PrivateKey`
  - `BIO_free`
- Keep the current public contracts:
  - `GenerateSelfSigned(...)` fails with controlled `ESSLCertError`
  - `TryGenerateSelfSigned(...)` / `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `False`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_bio_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_bio_contract -FEtmp/cert_utils_generate_selfsigned_bio_contract -otmp/cert_utils_generate_selfsigned_bio_contract/test_cert_utils_generate_selfsigned_bio_contract tests/test_cert_utils_generate_selfsigned_bio_contract.pas && ./tmp/cert_utils_generate_selfsigned_bio_contract/test_cert_utils_generate_selfsigned_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-cert-utils-generate-selfsigned-bio-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed generation contract passes without `EAccessViolation`
- helper-missing export paths fail with controlled certificate exceptions or Try-wrapper `False`
- full module compile remains green
