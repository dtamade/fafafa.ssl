# Certificate Utils GenerateSelfSigned Private-Key Export Post-Success Cleanup Family Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Close the remaining `GenerateSelfSigned(...)` private-key PEM export post-success cleanup family so already-materialized certificate/key PEM output survives late `BIO_free(...)` loss after the successful private-key readback, instead of being translated into a false failure.

**Architecture:** Keep this batch narrow and successful-path focused. Add one family-level contract around direct and Try self-signed generation, then harden only the private-key PEM export cleanup boundary so entry-missing `BIO_free` still fails as before, while delayed-loss after both PEM outputs are already materialized no longer overturns a successful result. Reuse the broad BIO entry-missing contract as the adjacent baseline, and realign the older wrapper-based private-key `BIO_free` test because it already exercises the same delayed-loss line.

**Tech Stack:** Free Pascal, OpenSSL loader stubs, focused Pascal contract tests

---

### Task 1: RED - Reproduce the self-signed private-key export post-success cleanup gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/test_cert_utils_generate_selfsigned_bio_contract.pas`
- Reference: `tests/test_cert_utils_generate_selfsigned_private_key_bio_free_symbol_contract.pas`
- Reference: `tests/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract.pas`

**Step 1: Write the failing test**

- Add one focused family-level contract test that:
  - initializes OpenSSL and loads `Core/BIO/X509/X509v3/PEM/EVP`
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - uses a delayed-loss `BIO_read(...)` wrapper so `BIO_free(...)` disappears only after the successful second private-key export readback
  - asserts direct `GenerateSelfSigned(...)` must not raise, must return `True`, and must preserve non-empty PEM outputs
  - asserts `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` must not raise, must return `True`, and must preserve non-empty PEM outputs

**Step 2: Run test to verify it fails**

Run:
`mkdir -p tmp/cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract -FEtmp/cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract -otmp/cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract/test_cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract tests/test_cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract.pas && ./tmp/cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract/test_cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract`

Expected:
- FAIL because direct `GenerateSelfSigned(...)` still raises a controlled cleanup error after both PEM outputs are already materialized
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing but return `False` and clear outputs in that delayed-loss scenario

### Task 2: GREEN - Minimal self-signed private-key export post-success cleanup preservation

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Step 1: Write minimal implementation**

- Harden only the private-key PEM export cleanup boundary after successful readback in `GenerateSelfSigned(...)`:
  - `finally BIO_free(LBIO)` for the private-key PEM export block
- Preserve existing contracts:
  - if `BIO_free` is unavailable before PEM outputs are materialized, direct `GenerateSelfSigned(...)` still fails through controlled `ESSLCertError`
  - once both PEM outputs are already materialized, delayed private-key export cleanup loss no longer flips success into failure
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `True` when the direct result survives cleanup loss
  - certificate PEM export cleanup and outer cleanup families stay untouched

### Task 3: Verification

**Files:**
- Verify: `tests/test_cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract.pas`
- Verify: `tests/test_cert_utils_generate_selfsigned_bio_contract.pas`
- Modify: `tests/test_cert_utils_generate_selfsigned_private_key_bio_free_symbol_contract.pas`
- Verify: `tests/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract.pas`
- Verify: `src/fafafa.ssl.cert.utils.pas`
- Verify: `task_plan.md`
- Verify: `findings.md`
- Verify: `progress.md`

**Step 1: Run focused family regression**

Run:
`mkdir -p tmp/cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract -FEtmp/cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract -otmp/cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract/test_cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract tests/test_cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract.pas && ./tmp/cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract/test_cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract`

Expected:
- PASS with no raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` preserves already-materialized outputs in the targeted delayed-loss scenario
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` stay non-throwing, return `True`, and preserve outputs

**Step 2: Run adjacent focused regressions**

Run:
- `mkdir -p tmp/test_cert_utils_generate_selfsigned_bio_contract && fpc -B -Fu./src -FUtmp/test_cert_utils_generate_selfsigned_bio_contract -FEtmp/test_cert_utils_generate_selfsigned_bio_contract -otmp/test_cert_utils_generate_selfsigned_bio_contract/test_cert_utils_generate_selfsigned_bio_contract tests/test_cert_utils_generate_selfsigned_bio_contract.pas && ./tmp/test_cert_utils_generate_selfsigned_bio_contract/test_cert_utils_generate_selfsigned_bio_contract`
- `mkdir -p tmp/test_cert_utils_generate_selfsigned_private_key_bio_free_symbol_contract && fpc -B -Fu./src -FUtmp/test_cert_utils_generate_selfsigned_private_key_bio_free_symbol_contract -FEtmp/test_cert_utils_generate_selfsigned_private_key_bio_free_symbol_contract -otmp/test_cert_utils_generate_selfsigned_private_key_bio_free_symbol_contract/test_cert_utils_generate_selfsigned_private_key_bio_free_symbol_contract tests/test_cert_utils_generate_selfsigned_private_key_bio_free_symbol_contract.pas && ./tmp/test_cert_utils_generate_selfsigned_private_key_bio_free_symbol_contract/test_cert_utils_generate_selfsigned_private_key_bio_free_symbol_contract`
- `mkdir -p tmp/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract && fpc -B -Fu./src -FUtmp/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract -FEtmp/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract -otmp/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract tests/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract.pas && ./tmp/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract/test_cert_utils_generate_selfsigned_post_success_cleanup_family_contract`

Expected:
- entry-missing BIO helper contract still passes
- the legacy wrapper-based private-key `BIO_free` focused contract is upgraded to preserved-success semantics and passes
- the broader outer post-success cleanup family stays green

**Step 3: Run full compile and diff hygiene**

Run:
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-25-cert-utils-generate-selfsigned-private-key-export-post-success-cleanup-family.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_private_key_export_post_success_cleanup_family_contract.pas tests/test_cert_utils_generate_selfsigned_private_key_bio_free_symbol_contract.pas task_plan.md findings.md progress.md`

Expected:
- full module compile remains green
- diff hygiene remains clean
