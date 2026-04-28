# Certificate Utils Fingerprint Post-Success Cleanup Family Plan

**Goal:** Close the remaining post-success cleanup family in `TCertificateUtils.GetFingerprint(...)` so an already-materialized SHA-256 fingerprint survives late cleanup-helper loss instead of collapsing into an access violation or a false failure.

**Architecture:** Keep this batch narrow:

- add one focused family-level contract test around `GetFingerprint(...)` / `TryGetFingerprint(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve the existing controlled-exception contract when prerequisite helpers are unavailable before fingerprint materialization
- close only the remaining delayed-loss cleanup helpers after fingerprint success:
  - `GetFingerprint(...)` cleanup `X509_free(LCert)` after successful `X509_digest(...)` and hex conversion
  - `GetFingerprint(...)` outer cleanup `BIO_free(LBIO)` after successful `X509_free(LCert)`
- do not redesign `X509_digest`, `EVP_sha256`, conversion helpers, generation helpers, or broader PEM/BIO entry guards

## Task 1: RED - Reproduce the fingerprint post-success cleanup family gaps

**Files:**
- Add: `tests/test_cert_utils_fingerprint_post_success_cleanup_family_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/test_cert_utils_fingerprint_x509_digest_symbol_contract.pas`
- Reference: `tests/test_cert_utils_fingerprint_evp_sha256_symbol_contract.pas`

**Steps:**
- Write one focused family-level contract test that:
  - initializes OpenSSL and loads `Core/BIO/X509/PEM/EVP`
  - loads a real PEM certificate fixture and warms a normal fingerprint result
  - uses delayed-loss wrappers so cleanup helpers disappear only after fingerprint success:
    - clear `X509_free` only after a successful `X509_digest(...)` call has already produced the hash bytes
    - clear `BIO_free` from an `X509_free(...)` wrapper after certificate cleanup succeeds
  - asserts direct `GetFingerprint(...)` must not raise and must preserve the already-materialized fingerprint string
  - asserts `TryGetFingerprint(...)` must not raise, must return `True`, and must preserve the same fingerprint string
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal post-success cleanup guards

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Harden only the actual delayed-loss cleanup sites after fingerprint success:
  - guard `X509_free(LCert)` inside `GetFingerprint(...)`
  - guard outer `BIO_free(LBIO)` inside `GetFingerprint(...)`
- Preserve existing contracts:
  - once the fingerprint string is already materialized, `GetFingerprint(...)` keeps that fingerprint and does not raise on cleanup helper loss
  - `TryGetFingerprint(...)` remains non-throwing and returns `True` when the direct fingerprint survives cleanup loss
  - earlier prerequisite helper loss (`BIO_new_mem_buf`, `PEM_read_bio_X509`, `X509_digest`, `EVP_sha256`, entry `BIO_free`) stays on the existing controlled-exception / `False` path

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_fingerprint_post_success_cleanup_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_fingerprint_post_success_cleanup_family_contract -FEtmp/cert_utils_fingerprint_post_success_cleanup_family_contract -otmp/cert_utils_fingerprint_post_success_cleanup_family_contract/test_cert_utils_fingerprint_post_success_cleanup_family_contract tests/test_cert_utils_fingerprint_post_success_cleanup_family_contract.pas && ./tmp/cert_utils_fingerprint_post_success_cleanup_family_contract/test_cert_utils_fingerprint_post_success_cleanup_family_contract`
- `mkdir -p tmp/cert_utils_conversion_bio_contract && fpc -B -Fu./src -FUtmp/cert_utils_conversion_bio_contract -FEtmp/cert_utils_conversion_bio_contract -otmp/cert_utils_conversion_bio_contract/test_cert_utils_conversion_bio_contract tests/test_cert_utils_conversion_bio_contract.pas && ./tmp/cert_utils_conversion_bio_contract/test_cert_utils_conversion_bio_contract`
- `mkdir -p tmp/cert_utils_fingerprint_x509_digest_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_fingerprint_x509_digest_symbol_contract -FEtmp/cert_utils_fingerprint_x509_digest_symbol_contract -otmp/cert_utils_fingerprint_x509_digest_symbol_contract/test_cert_utils_fingerprint_x509_digest_symbol_contract tests/test_cert_utils_fingerprint_x509_digest_symbol_contract.pas && ./tmp/cert_utils_fingerprint_x509_digest_symbol_contract/test_cert_utils_fingerprint_x509_digest_symbol_contract`
- `mkdir -p tmp/cert_utils_fingerprint_evp_sha256_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_fingerprint_evp_sha256_symbol_contract -FEtmp/cert_utils_fingerprint_evp_sha256_symbol_contract -otmp/cert_utils_fingerprint_evp_sha256_symbol_contract/test_cert_utils_fingerprint_evp_sha256_symbol_contract tests/test_cert_utils_fingerprint_evp_sha256_symbol_contract.pas && ./tmp/cert_utils_fingerprint_evp_sha256_symbol_contract/test_cert_utils_fingerprint_evp_sha256_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-fingerprint-post-success-cleanup-family.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_fingerprint_post_success_cleanup_family_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- the focused family-level cleanup contract passes without raw `EAccessViolation`
- `GetFingerprint(...)` preserves an already-materialized fingerprint across the targeted cleanup-loss scenarios
- `TryGetFingerprint(...)` remains non-throwing, returns `True`, and preserves the same fingerprint in those scenarios
- the earlier conversion BIO guard and focused fingerprint digest/algorithm symbol contracts still pass
- full module compile remains green
