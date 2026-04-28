# Certificate Utils GenerateSigned PEM Export BIO_new Nil-Result Family Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make `TCertificateUtils.GenerateSigned(...)` fail at the local PEM export BIO constructor boundary when `BIO_new(...)` stays assigned but returns `nil`, preserving clear direct errors and stable `TryGenerateSigned(...)` behavior.

**Architecture:** Keep this batch family-sized and narrow. Add one focused contract test that covers both `GenerateSigned(...)` PEM export constructor sites:

- certificate PEM export `LBIO := BIO_new(LBIOMethod)`
- private-key PEM export `LBIO := BIO_new(LBIOMethod)`

The RED should simulate a real nil-result constructor while the symbol remains assigned, so the existing helper gates still pass. GREEN is limited to two local `LBIO = nil` guards in `src/fafafa.ssl.cert.utils.pas`, matching the already-closed `GenerateSelfSigned(...)` pattern.

**Tech Stack:** Free Pascal, OpenSSL loader wrappers, focused contract test

---

## Task 1: RED - Reproduce the signed PEM export constructor nil-result gap

**Files:**
- Add: `tests/test_cert_utils_generate_signed_pem_export_bio_new_nil_result_family_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write one focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by `GenerateSigned(...)`
  - warms a normal CA generation + signed leaf generation path
  - installs a `BIO_new` wrapper that stays assigned but returns `nil` on:
    - the first `BIO_new(...)` call inside `GenerateSigned(...)` for certificate PEM export
    - the second `BIO_new(...)` call inside `GenerateSigned(...)` for private-key PEM export
  - asserts direct `TCertificateUtils.GenerateSigned(...)` raises `ESSLCertError`
  - asserts the direct error messages are the constructor-specific messages:
    - `Failed to create BIO for certificate export`
    - `Failed to create BIO for key export`
  - asserts `TCertificateUtils.TryGenerateSigned(...)` does not raise, returns `False`, and clears outputs in both scenarios
- Run the focused test and confirm the current source fails.

## Task 2: GREEN - Minimal signed PEM export constructor nil-result guards

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add `if LBIO = nil then raise ESSLCertError.Create(...)` immediately after the certificate PEM export `BIO_new(LBIOMethod)` call.
- Add `if LBIO = nil then raise ESSLCertError.Create(...)` immediately after the private-key PEM export `BIO_new(LBIOMethod)` call.
- Reuse the existing constructor-specific messages so this family aligns with `GenerateSelfSigned(...)`.
- Keep successful CA-signed generation unchanged when `BIO_new(...)` returns a valid BIO.
- Do not redesign any later PEM write/read/cleanup helpers.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_signed_pem_export_bio_new_nil_result_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_pem_export_bio_new_nil_result_family_contract -FEtmp/cert_utils_generate_signed_pem_export_bio_new_nil_result_family_contract -otmp/cert_utils_generate_signed_pem_export_bio_new_nil_result_family_contract/test_cert_utils_generate_signed_pem_export_bio_new_nil_result_family_contract tests/test_cert_utils_generate_signed_pem_export_bio_new_nil_result_family_contract.pas && ./tmp/cert_utils_generate_signed_pem_export_bio_new_nil_result_family_contract/test_cert_utils_generate_signed_pem_export_bio_new_nil_result_family_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-generate-signed-pem-export-bio-new-nil-result-family.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_signed_pem_export_bio_new_nil_result_family_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused contract first fails on current source because the direct error message does not stop at the nil-result constructor boundary
- after the fix, both constructor scenarios pass with constructor-specific direct errors
- `TryGenerateSigned(...)` remains non-throwing, returns `False`, and clears outputs
- full module compile remains green
