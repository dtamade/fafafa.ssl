# Certificate Utils GetInfo BIO Guard Plan

**Goal:** Make `TCertificateUtils.GetInfo(...)` fail according to its existing silent-degrade contract when required PEM read BIO helpers are unavailable, instead of dereferencing nil function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GetInfo(...)` / `TryGetInfo(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful metadata extraction behavior when helpers are available
- do not redesign `VerifyChain(...)`, certificate generation, or broader info parsing behavior

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_cert_utils_getinfo_bio_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`
- Reference: `src/fafafa.ssl.openssl.api.pem.pas`
- Reference: `src/fafafa.ssl.openssl.api.x509.pas`

**Steps:**
- Write a focused contract test that:
  - initializes the OpenSSL library and loads `Core/BIO/X509/PEM/EVP`
  - loads a valid certificate PEM fixture and warms a normal `GetInfo(...)` path
  - temporarily clears representative PEM read helpers:
    - `BIO_new_mem_buf`
    - `BIO_free`
  - asserts `GetInfo(...)` must not raise on helper unavailability
  - asserts helper-missing calls degrade to an empty `TCertInfo` while keeping `SubjectAltNames` allocated
  - asserts `TryGetInfo(...)` does not raise and returns sanitized output
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal GetInfo guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add an early-return guard so `GetInfo(...)` requires:
  - `BIO_new_mem_buf`
  - `PEM_read_bio_X509`
  - `BIO_free`
- Preserve existing `TCertInfo` initialization:
  - zeroed fields on failure
  - allocated `SubjectAltNames`
  - unchanged successful metadata extraction when helpers are present
- Avoid changing `TryGetInfo(...)` unless strictly required by the focused contract.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_getinfo_bio_contract && fpc -B -Fu./src -FUtmp/cert_utils_getinfo_bio_contract -FEtmp/cert_utils_getinfo_bio_contract -otmp/cert_utils_getinfo_bio_contract/test_cert_utils_getinfo_bio_contract tests/test_cert_utils_getinfo_bio_contract.pas && ./tmp/cert_utils_getinfo_bio_contract/test_cert_utils_getinfo_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-cert-utils-getinfo-bio-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_getinfo_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused `GetInfo` contract passes without `EAccessViolation`
- missing PEM read helpers degrade to empty info instead of crashing
- full module compile remains green
