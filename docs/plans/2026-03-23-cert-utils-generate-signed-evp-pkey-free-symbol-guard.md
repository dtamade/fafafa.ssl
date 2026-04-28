# Certificate Utils GenerateSigned EVP_PKEY_free Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSigned(...)` preserve its existing exception-based contract when `EVP_PKEY_free` becomes unavailable on the outer private-key cleanup path, instead of dereferencing nil EVP cleanup helpers after CA-signed certificate generation and PEM export have already completed.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSigned(...)` and `TryGenerateSigned(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful CA-signed certificate generation behavior when `EVP_PKEY_free` is available
- keep this as a single-symbol batch for `EVP_PKEY_free`
- because `GenerateSigned(...)` uses the same symbol twice in an uninterrupted cleanup chain, cover both cleanup call sites for that same symbol in this batch:
  - `EVP_PKEY_free(LKey)`
  - `EVP_PKEY_free(LCAKey)`
- do not redesign `GenerateSelfSigned(...)`, `X509_free`, or broader cleanup ordering logic

## Task 1: RED - Reproduce the signed outer private-key cleanup gap

**Files:**
- Add: `tests/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by `GenerateSigned(...)`
  - warms a normal CA generation + `GenerateSigned(...)` path
  - temporarily clears `EVP_PKEY_free`
  - asserts direct `TCertificateUtils.GenerateSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSigned(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal signed outer private-key cleanup guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add local `Assigned(EVP_PKEY_free)` guards inside `TCertificateUtils.GenerateSigned(...)` around the outer private-key cleanup path before:
  - `EVP_PKEY_free(LKey)`
  - `EVP_PKEY_free(LCAKey)`
- Preserve current behavior:
  - direct `GenerateSigned(...)` raises controlled `ESSLCertError` when private-key cleanup helpers are unavailable
  - successful generation remains unchanged when `EVP_PKEY_free` is available
  - `TryGenerateSigned(...)` remains non-throwing and returns `False`
  - later `X509_free(LCACert)` cleanup stays untouched for a separate isolated batch

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_signed_evp_pkey_free_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_evp_pkey_free_symbol_contract -FEtmp/cert_utils_generate_signed_evp_pkey_free_symbol_contract -otmp/cert_utils_generate_signed_evp_pkey_free_symbol_contract/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract tests/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract.pas && ./tmp/cert_utils_generate_signed_evp_pkey_free_symbol_contract/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-23-cert-utils-generate-signed-evp-pkey-free-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_signed_evp_pkey_free_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused signed-generation contract passes without raw `EAccessViolation`
- direct `GenerateSigned(...)` raises `ESSLCertError` when `EVP_PKEY_free` is unavailable on the outer private-key cleanup path
- `TryGenerateSigned(...)` returns `False` and clears outputs
