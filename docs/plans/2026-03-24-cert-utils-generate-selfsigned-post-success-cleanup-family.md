# Certificate Utils GenerateSelfSigned Post-Success Cleanup Family Plan

**Goal:** Close the remaining post-success cleanup family in `TCertificateUtils.GenerateSelfSigned(...)` so already-materialized certificate/key PEM output survives late outer-cleanup helper loss instead of being translated into a false failure.

**Architecture:** Keep this batch narrow:

- add one focused family-level contract test around:
  - `GenerateSelfSigned(...)`
  - `TryGenerateSelfSigned(...)`
  - `TryGenerateSelfSignedSimple(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve the existing controlled-failure contract when cleanup helpers are unavailable before generation starts
- close only the delayed-loss cleanup helpers after both PEM outputs are already materialized:
  - `GenerateSelfSigned(...)` outer certificate cleanup `X509_free(LCert)` after successful certificate + key PEM export
  - `GenerateSelfSigned(...)` outer private-key cleanup `EVP_PKEY_free(LKey)` after successful certificate cleanup
- do not redesign earlier keygen, extension, PEM export, or `GenerateSigned(...)` logic

## Task 1: RED - Reproduce the post-success cleanup family gaps

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/test_cert_utils_generate_selfsigned_x509_free_symbol_contract.pas`
- Reference: `tests/test_cert_utils_generate_selfsigned_evp_pkey_free_symbol_contract.pas`

**Steps:**
- Write one focused family-level contract test that:
  - initializes OpenSSL and loads `Core/BIO/X509/X509v3/PEM/EVP`
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - uses delayed-loss wrappers so outer cleanup helpers disappear only after both PEM outputs are already generated:
    - clear `X509_free` from a `BIO_read(...)` wrapper after the successful second PEM read (private-key export)
    - clear `EVP_PKEY_free` from an `X509_free(...)` wrapper after certificate cleanup succeeds
  - asserts direct `GenerateSelfSigned(...)` must not raise, must return `True`, and must preserve non-empty PEM outputs
  - asserts `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` must not raise, must return `True`, and must preserve non-empty PEM outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal post-success cleanup preservation

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Harden only the actual delayed-loss outer cleanup sites after self-signed PEM success:
  - `X509_free(LCert)`
  - `EVP_PKEY_free(LKey)`
- Preserve existing contracts:
  - if those helpers were already missing before generation, direct `GenerateSelfSigned(...)` still fails through controlled `ESSLCertError`
  - once both PEM outputs are already materialized, delayed cleanup-helper loss no longer flips success into failure
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `True` when the direct result survives cleanup loss
  - earlier PEM export cleanup families stay untouched

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_post_success_cleanup_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_post_success_cleanup_family_contract -FEtmp/cert_utils_generate_selfsigned_post_success_cleanup_family_contract -otmp/cert_utils_generate_selfsigned_post_success_cleanup_family_contract/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract tests/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract.pas && ./tmp/cert_utils_generate_selfsigned_post_success_cleanup_family_contract/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract`
- `mkdir -p tmp/test_cert_utils_generate_selfsigned_x509_free_symbol_contract && fpc -B -Fu./src -FUtmp/test_cert_utils_generate_selfsigned_x509_free_symbol_contract -FEtmp/test_cert_utils_generate_selfsigned_x509_free_symbol_contract -otmp/test_cert_utils_generate_selfsigned_x509_free_symbol_contract/test_cert_utils_generate_selfsigned_x509_free_symbol_contract tests/test_cert_utils_generate_selfsigned_x509_free_symbol_contract.pas && ./tmp/test_cert_utils_generate_selfsigned_x509_free_symbol_contract/test_cert_utils_generate_selfsigned_x509_free_symbol_contract`
- `mkdir -p tmp/test_cert_utils_generate_selfsigned_evp_pkey_free_symbol_contract && fpc -B -Fu./src -FUtmp/test_cert_utils_generate_selfsigned_evp_pkey_free_symbol_contract -FEtmp/test_cert_utils_generate_selfsigned_evp_pkey_free_symbol_contract -otmp/test_cert_utils_generate_selfsigned_evp_pkey_free_symbol_contract/test_cert_utils_generate_selfsigned_evp_pkey_free_symbol_contract tests/test_cert_utils_generate_selfsigned_evp_pkey_free_symbol_contract.pas && ./tmp/test_cert_utils_generate_selfsigned_evp_pkey_free_symbol_contract/test_cert_utils_generate_selfsigned_evp_pkey_free_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-generate-selfsigned-post-success-cleanup-family.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- the focused family-level contract passes without raw `EAccessViolation`
- `GenerateSelfSigned(...)` preserves already-materialized PEM outputs across the targeted outer-cleanup delayed-loss scenarios
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing, return `True`, and preserve PEM outputs in those scenarios
- the older entry-missing `X509_free` / `EVP_PKEY_free` symbol contracts still pass
- full module compile remains green
