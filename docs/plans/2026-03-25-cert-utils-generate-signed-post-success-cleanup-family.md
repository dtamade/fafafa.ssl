# Certificate Utils GenerateSigned Post-Success Cleanup Family Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Close the remaining post-success cleanup family in `TCertificateUtils.GenerateSigned(...)` so already-materialized certificate/key PEM output survives late outer-cleanup helper loss instead of being translated into a false failure.

**Architecture:** Keep this batch narrow and successful-path focused. Add one family-level contract around direct and Try generation, then harden only the outer signed cleanup sites so entry-missing helpers still fail as before, while delayed-loss after successful PEM materialization no longer overturns a successful result. Reuse the existing entry-missing symbol contracts as adjacent regression coverage rather than reopening those lines.

**Tech Stack:** Free Pascal, OpenSSL loader stubs, focused Pascal contract tests

---

### Task 1: RED - Reproduce the signed post-success cleanup family gaps

**Files:**
- Add: `tests/test_cert_utils_generate_signed_post_success_cleanup_family_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/test_cert_utils_generate_signed_x509_free_symbol_contract.pas`
- Reference: `tests/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract.pas`
- Reference: `tests/test_cert_utils_generate_signed_ca_x509_free_symbol_contract.pas`

**Step 1: Write the failing test**

- Add one focused family-level contract test that:
  - initializes OpenSSL and loads `Core/BIO/X509/X509v3/PEM/EVP`
  - warms a normal CA generation + `GenerateSigned(...)` path
  - uses delayed-loss wrappers so outer cleanup helpers disappear only after certificate/key PEM outputs are already generated:
    - clear `X509_free` from a `BIO_read(...)` wrapper after the successful second PEM read to expose the leaf-certificate cleanup
    - clear `EVP_PKEY_free` from an `X509_free(...)` wrapper after the leaf certificate cleanup succeeds to expose the leaf private-key cleanup
    - clear `EVP_PKEY_free` from an `EVP_PKEY_free(...)` wrapper after the first key cleanup succeeds to expose the CA private-key cleanup
    - clear `X509_free` from an `EVP_PKEY_free(...)` wrapper after the second key cleanup succeeds to expose the CA certificate cleanup
  - asserts direct `GenerateSigned(...)` must not raise, must return `True`, and must preserve non-empty PEM outputs
  - asserts `TryGenerateSigned(...)` must not raise, must return `True`, and must preserve non-empty PEM outputs

**Step 2: Run test to verify it fails**

Run:
`mkdir -p tmp/cert_utils_generate_signed_post_success_cleanup_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_post_success_cleanup_family_contract -FEtmp/cert_utils_generate_signed_post_success_cleanup_family_contract -otmp/cert_utils_generate_signed_post_success_cleanup_family_contract/test_cert_utils_generate_signed_post_success_cleanup_family_contract tests/test_cert_utils_generate_signed_post_success_cleanup_family_contract.pas && ./tmp/cert_utils_generate_signed_post_success_cleanup_family_contract/test_cert_utils_generate_signed_post_success_cleanup_family_contract`

Expected:
- FAIL because direct `GenerateSigned(...)` still raises controlled cleanup errors after PEM outputs are already materialized
- `TryGenerateSigned(...)` remains non-throwing but returns `False` and clears outputs in those delayed-loss scenarios

### Task 2: GREEN - Minimal signed post-success cleanup preservation

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Step 1: Write minimal implementation**

- Harden only the actual delayed-loss outer cleanup sites after signed PEM success:
  - `X509_free(LCert)`
  - `EVP_PKEY_free(LKey)`
  - `EVP_PKEY_free(LCAKey)`
  - `X509_free(LCACert)`
- Preserve existing contracts:
  - if those helpers were already missing before generation, direct `GenerateSigned(...)` still fails through controlled `ESSLCertError`
  - once both PEM outputs are already materialized, delayed cleanup-helper loss no longer flips success into failure
  - `TryGenerateSigned(...)` remains non-throwing and returns `True` when the direct result survives cleanup loss
  - earlier CA-load, PEM export, and entry-missing cleanup families stay untouched

### Task 3: Verification

**Files:**
- Verify: `tests/test_cert_utils_generate_signed_post_success_cleanup_family_contract.pas`
- Verify: `tests/test_cert_utils_generate_signed_x509_free_symbol_contract.pas`
- Verify: `tests/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract.pas`
- Modify: `tests/test_cert_utils_generate_signed_ca_x509_free_symbol_contract.pas`
- Verify: `src/fafafa.ssl.cert.utils.pas`
- Verify: `task_plan.md`
- Verify: `findings.md`
- Verify: `progress.md`

**Step 1: Run focused family regression**

Run:
`mkdir -p tmp/cert_utils_generate_signed_post_success_cleanup_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_post_success_cleanup_family_contract -FEtmp/cert_utils_generate_signed_post_success_cleanup_family_contract -otmp/cert_utils_generate_signed_post_success_cleanup_family_contract/test_cert_utils_generate_signed_post_success_cleanup_family_contract tests/test_cert_utils_generate_signed_post_success_cleanup_family_contract.pas && ./tmp/cert_utils_generate_signed_post_success_cleanup_family_contract/test_cert_utils_generate_signed_post_success_cleanup_family_contract`

Expected:
- PASS with no raw `EAccessViolation`
- direct `GenerateSigned(...)` preserves already-materialized outputs in the targeted delayed-loss scenarios
- `TryGenerateSigned(...)` stays non-throwing, returns `True`, and preserves outputs

**Step 2: Run adjacent focused regressions**

Run:
- `mkdir -p tmp/test_cert_utils_generate_signed_x509_free_symbol_contract && fpc -B -Fu./src -FUtmp/test_cert_utils_generate_signed_x509_free_symbol_contract -FEtmp/test_cert_utils_generate_signed_x509_free_symbol_contract -otmp/test_cert_utils_generate_signed_x509_free_symbol_contract/test_cert_utils_generate_signed_x509_free_symbol_contract tests/test_cert_utils_generate_signed_x509_free_symbol_contract.pas && ./tmp/test_cert_utils_generate_signed_x509_free_symbol_contract/test_cert_utils_generate_signed_x509_free_symbol_contract`
- `mkdir -p tmp/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract && fpc -B -Fu./src -FUtmp/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract -FEtmp/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract -otmp/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract tests/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract.pas && ./tmp/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract`
- `mkdir -p tmp/test_cert_utils_generate_signed_ca_x509_free_symbol_contract && fpc -B -Fu./src -FUtmp/test_cert_utils_generate_signed_ca_x509_free_symbol_contract -FEtmp/test_cert_utils_generate_signed_ca_x509_free_symbol_contract -otmp/test_cert_utils_generate_signed_ca_x509_free_symbol_contract/test_cert_utils_generate_signed_ca_x509_free_symbol_contract tests/test_cert_utils_generate_signed_ca_x509_free_symbol_contract.pas && ./tmp/test_cert_utils_generate_signed_ca_x509_free_symbol_contract/test_cert_utils_generate_signed_ca_x509_free_symbol_contract`

Expected:
- leaf `X509_free` entry-missing contract still passes
- `EVP_PKEY_free` entry-missing contract still passes
- the legacy CA `X509_free` wrapper-based focused contract is upgraded to preserved-success semantics and passes

**Step 3: Run full compile and diff hygiene**

Run:
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-25-cert-utils-generate-signed-post-success-cleanup-family.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_signed_post_success_cleanup_family_contract.pas tests/test_cert_utils_generate_signed_ca_x509_free_symbol_contract.pas task_plan.md findings.md progress.md`

Expected:
- full module compile remains green
- diff hygiene remains clean
