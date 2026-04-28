# Certificate Utils Conversion Post-Success Cleanup Family Plan

**Goal:** Close the remaining post-success cleanup family in `TCertificateUtils.PEMToDER(...)` and `TCertificateUtils.DERToPEM(...)` so already-materialized conversion output survives late helper loss instead of crashing on nil cleanup dereferences.

**Architecture:** Keep this batch narrow:

- add one focused family-level contract test around `PEMToDER(...)` / `TryPEMToDER(...)` and `DERToPEM(...)` / `TryDERToPEM(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve the existing successful conversion outputs when helpers remain available
- close only the remaining reachable delayed-loss cleanup helpers after conversion success:
  - `PEMToDER(...)` cleanup `X509_free(LCert)` after successful `i2d_X509(...)`
  - `PEMToDER(...)` outer cleanup `BIO_free(LBIO)` after successful `X509_free(LCert)`
  - `DERToPEM(...)` inner cleanup `BIO_free(LBIO)` after successful `PEM_write_bio_X509(...)` and PEM extraction
  - `DERToPEM(...)` outer cleanup `X509_free(LCert)` after successful inner `BIO_free(LBIO)`
- do not redesign `GetFingerprint(...)`, generation helpers, `GetInfo(...)`, `VerifyChain(...)`, or broader conversion entry guards

## Task 1: RED - Reproduce the post-success cleanup family gaps

**Files:**
- Add: `tests/test_cert_utils_conversion_post_success_cleanup_family_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/test_cert_utils_conversion_bio_contract.pas`

**Steps:**
- Write one focused family-level contract test that:
  - initializes OpenSSL and loads `Core/BIO/X509/PEM`
  - loads a real PEM certificate fixture and warms both normal conversions:
    - `PEMToDER(...)`
    - `DERToPEM(...)`
  - uses delayed-loss wrappers so cleanup helpers disappear only after output success:
    - clear `X509_free` only after the successful `i2d_X509(...)` encode call that materializes DER output
    - clear `BIO_free` from an `X509_free(...)` wrapper after PEMToDER certificate cleanup succeeds
    - clear `BIO_free` only after `PEM_write_bio_X509(...)` succeeds and before `DERToPEM(...)` inner cleanup runs
    - track the `DERToPEM(...)` export BIO via a `BIO_new(...)` wrapper, then clear `X509_free` when that tracked BIO is freed
  - asserts direct converters must not raise and must preserve the already-materialized output
  - asserts `TryPEMToDER(...)` / `TryDERToPEM(...)` must not raise, must return `True`, and must preserve the same output
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal post-success cleanup guards

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Harden only the actual delayed-loss cleanup sites after conversion success:
  - guard `X509_free(LCert)` inside `PEMToDER(...)`
  - guard outer `BIO_free(LBIO)` inside `PEMToDER(...)`
  - guard inner `BIO_free(LBIO)` inside `DERToPEM(...)`
  - guard outer `X509_free(LCert)` inside `DERToPEM(...)`
- Preserve existing contracts:
  - once DER output is already materialized, `PEMToDER(...)` keeps that output and does not raise on cleanup helper loss
  - once PEM output is already materialized, `DERToPEM(...)` keeps that output and does not raise on cleanup helper loss
  - `TryPEMToDER(...)` / `TryDERToPEM(...)` remain non-throwing and return `True` when the direct conversion output survives cleanup loss

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_conversion_post_success_cleanup_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_conversion_post_success_cleanup_family_contract -FEtmp/cert_utils_conversion_post_success_cleanup_family_contract -otmp/cert_utils_conversion_post_success_cleanup_family_contract/test_cert_utils_conversion_post_success_cleanup_family_contract tests/test_cert_utils_conversion_post_success_cleanup_family_contract.pas && ./tmp/cert_utils_conversion_post_success_cleanup_family_contract/test_cert_utils_conversion_post_success_cleanup_family_contract`
- `mkdir -p tmp/cert_utils_conversion_bio_contract && fpc -B -Fu./src -FUtmp/cert_utils_conversion_bio_contract -FEtmp/cert_utils_conversion_bio_contract -otmp/cert_utils_conversion_bio_contract/test_cert_utils_conversion_bio_contract tests/test_cert_utils_conversion_bio_contract.pas && ./tmp/cert_utils_conversion_bio_contract/test_cert_utils_conversion_bio_contract`
- `mkdir -p tmp/cert_utils_pemtoder_i2d_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_pemtoder_i2d_symbol_contract -FEtmp/cert_utils_pemtoder_i2d_symbol_contract -otmp/cert_utils_pemtoder_i2d_symbol_contract/test_cert_utils_pemtoder_i2d_symbol_contract tests/test_cert_utils_pemtoder_i2d_symbol_contract.pas && ./tmp/cert_utils_pemtoder_i2d_symbol_contract/test_cert_utils_pemtoder_i2d_symbol_contract`
- `mkdir -p tmp/cert_utils_dertopem_d2i_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_dertopem_d2i_symbol_contract -FEtmp/cert_utils_dertopem_d2i_symbol_contract -otmp/cert_utils_dertopem_d2i_symbol_contract/test_cert_utils_dertopem_d2i_symbol_contract tests/test_cert_utils_dertopem_d2i_symbol_contract.pas && ./tmp/cert_utils_dertopem_d2i_symbol_contract/test_cert_utils_dertopem_d2i_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-conversion-post-success-cleanup-family.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_conversion_post_success_cleanup_family_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- the focused family-level cleanup contract passes without raw `EAccessViolation`
- `PEMToDER(...)` and `DERToPEM(...)` preserve already-materialized output across the targeted cleanup-loss scenarios
- `TryPEMToDER(...)` and `TryDERToPEM(...)` remain non-throwing, return `True`, and preserve the same output in those scenarios
- the earlier conversion BIO guard and the focused encode/decode symbol contracts still pass
- full module compile remains green
