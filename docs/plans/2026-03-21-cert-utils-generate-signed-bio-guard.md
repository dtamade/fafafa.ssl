# Certificate Utils GenerateSigned BIO Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSigned(...)` fail according to its existing exception-based contract when required CA PEM read or leaf PEM export helpers are unavailable, instead of dereferencing nil function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSigned(...)` and `TryGenerateSigned(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful CA-signed certificate generation behavior when helpers are available
- do not redesign `GenerateSelfSigned(...)`, `VerifyChain(...)`, or broader certificate generation logic

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_cert_utils_generate_signed_bio_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`
- Reference: `src/fafafa.ssl.openssl.api.pem.pas`

**Steps:**
- Write a focused contract test that:
  - initializes the OpenSSL library and loads `Core/BIO/X509/PEM/EVP`
  - warms a normal CA generation + `GenerateSigned(...)` path
  - temporarily clears representative CA-read helpers:
    - `BIO_new_mem_buf`
    - `PEM_read_bio_X509`
    - `PEM_read_bio_PrivateKey`
    - `BIO_free`
  - temporarily clears representative leaf-export helpers:
    - `BIO_new`
    - `BIO_s_mem`
    - `PEM_write_bio_X509`
    - `PEM_write_bio_PrivateKey`
    - `BIO_free`
  - asserts direct `GenerateSigned(...)` must raise controlled `ESSLCertError` instead of `EAccessViolation`
  - asserts `TryGenerateSigned(...)` does not raise and returns `False` with cleared outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal GenerateSigned guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add local guard checks so CA certificate load requires:
  - `BIO_new_mem_buf`
  - `PEM_read_bio_X509`
  - `BIO_free`
- Add local guard checks so CA private-key load requires:
  - `BIO_new_mem_buf`
  - `PEM_read_bio_PrivateKey`
  - `BIO_free`
- Reuse or extend existing export helper predicates so leaf export requires:
  - `BIO_new`
  - `BIO_s_mem`
  - `PEM_write_bio_X509`
  - `PEM_write_bio_PrivateKey`
  - `BIO_free`
- Keep the current public contracts:
  - `GenerateSigned(...)` fails with controlled `ESSLCertError`
  - `TryGenerateSigned(...)` remains non-throwing and returns `False`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_signed_bio_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_bio_contract -FEtmp/cert_utils_generate_signed_bio_contract -otmp/cert_utils_generate_signed_bio_contract/test_cert_utils_generate_signed_bio_contract tests/test_cert_utils_generate_signed_bio_contract.pas && ./tmp/cert_utils_generate_signed_bio_contract/test_cert_utils_generate_signed_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-cert-utils-generate-signed-bio-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_signed_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused CA-signed generation contract passes without `EAccessViolation`
- helper-missing CA-read or leaf-export paths fail with controlled certificate exceptions or Try-wrapper `False`
- full module compile remains green
