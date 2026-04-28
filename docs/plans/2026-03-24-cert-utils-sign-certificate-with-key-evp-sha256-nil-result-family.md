# Certificate Utils SignCertificateWithKey EVP_sha256 Nil-Result Family Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make `TCertificateUtils.SignCertificateWithKey(...)` fail cleanly when `EVP_sha256` stays assigned but returns `nil`, instead of silently succeeding on the RSA signing path through the later nil-digest fallback.

**Architecture:** Keep this batch helper-family sized and shared. Add one focused contract test that exercises the shared RSA signing helper through both public call sites:

- `TCertificateUtils.GenerateSelfSigned(...)`
- `TCertificateUtils.GenerateSigned(...)`

The RED simulates a real nil-result digest helper while the symbol remains assigned, so the earlier symbol guard still passes. GREEN stays local to `SignCertificateWithKey(...)` by caching the SHA-256 digest pointer once, requiring it to be non-nil before the preferred signing attempt, and preserving the existing nil-digest fallback only when the digest helper itself succeeded and the first signing attempt returned failure.

**Tech Stack:** Free Pascal, OpenSSL EVP/X509 helpers, focused contract test

---

## Task 1: RED - Reproduce the shared signing-helper nil-result gap

**Files:**
- Add: `tests/test_cert_utils_sign_certificate_with_key_evp_sha256_nil_result_family_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write one focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by certificate generation
  - warms a normal RSA self-signed path
  - warms a normal RSA CA-signed path
  - installs an `EVP_sha256` wrapper that remains assigned but returns `nil`
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` raises `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` do not raise, return `False`, and clear outputs
  - asserts direct `TCertificateUtils.GenerateSigned(...)` raises `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSigned(...)` does not raise, returns `False`, and clears outputs
- Run the focused contract and confirm the current source fails because generation still succeeds.

## Task 2: GREEN - Minimal shared digest-result guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local digest variable in `SignCertificateWithKey(...)`.
- After the Ed25519 early-return path and after the existing `Assigned(EVP_sha256)` guard, call `EVP_sha256()` once and require the returned digest pointer to be non-nil.
- Use the cached digest pointer for the preferred RSA/ECDSA `X509_sign(...)` call.
- Preserve the existing nil-digest fallback only when the digest helper returned a valid pointer and the preferred signing call itself failed.
- Do not redesign public error messages, Ed25519 signing, PEM export, or generation flow outside this helper.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_sign_certificate_with_key_evp_sha256_nil_result_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_sign_certificate_with_key_evp_sha256_nil_result_family_contract -FEtmp/cert_utils_sign_certificate_with_key_evp_sha256_nil_result_family_contract -otmp/cert_utils_sign_certificate_with_key_evp_sha256_nil_result_family_contract/test_cert_utils_sign_certificate_with_key_evp_sha256_nil_result_family_contract tests/test_cert_utils_sign_certificate_with_key_evp_sha256_nil_result_family_contract.pas && ./tmp/cert_utils_sign_certificate_with_key_evp_sha256_nil_result_family_contract/test_cert_utils_sign_certificate_with_key_evp_sha256_nil_result_family_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-sign-certificate-with-key-evp-sha256-nil-result-family.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_sign_certificate_with_key_evp_sha256_nil_result_family_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused contract first fails on current source because both direct generation paths still succeed when `EVP_sha256()` returns `nil`
- after the fix, both direct generation paths fail through controlled `ESSLCertError`
- `TryGenerateSelfSigned(...)`, `TryGenerateSelfSignedSimple(...)`, and `TryGenerateSigned(...)` remain non-throwing, return `False`, and clear outputs
- full module compile remains green
