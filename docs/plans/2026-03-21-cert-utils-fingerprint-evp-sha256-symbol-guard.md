# Certificate Utils Fingerprint EVP_sha256 Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GetFingerprint(...)` preserve its existing controlled-exception contract when `EVP_sha256` is unavailable, instead of dereferencing a nil EVP digest helper.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for `TCertificateUtils.GetFingerprint(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful fingerprint behavior when `EVP_sha256` is available
- do not redesign `TryGetFingerprint(...)`, `X509_digest`, conversion helpers, generation helpers, or broader PEM/BIO loading logic

## Task 1: RED - Reproduce the fingerprint EVP helper gap

**Files:**
- Add: `tests/test_cert_utils_fingerprint_evp_sha256_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/certificate/test_certs/signer_cert.pem`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads a valid certificate PEM fixture and warms a normal `GetFingerprint(...)` path
  - temporarily clears `EVP_sha256`
  - asserts direct `TCertificateUtils.GetFingerprint(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGetFingerprint(...)` must not raise, must return `False`, and must clear its output
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal fingerprint EVP helper guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(EVP_sha256)` guard inside `TCertificateUtils.GetFingerprint(...)` after PEM parsing succeeds and before calling `EVP_sha256()`
- Preserve current behavior:
  - empty input still raises an invalid-parameter certificate exception
  - missing PEM/BIO read helpers still raise a controlled certificate exception through the existing guard path
  - `TryGetFingerprint(...)` remains non-throwing and returns `False`
- Leave `X509_digest` guard untouched.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_fingerprint_evp_sha256_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_fingerprint_evp_sha256_symbol_contract -FEtmp/cert_utils_fingerprint_evp_sha256_symbol_contract -otmp/cert_utils_fingerprint_evp_sha256_symbol_contract/test_cert_utils_fingerprint_evp_sha256_symbol_contract tests/test_cert_utils_fingerprint_evp_sha256_symbol_contract.pas && ./tmp/cert_utils_fingerprint_evp_sha256_symbol_contract/test_cert_utils_fingerprint_evp_sha256_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-cert-utils-fingerprint-evp-sha256-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_fingerprint_evp_sha256_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused fingerprint contract passes without raw `EAccessViolation`
- direct `GetFingerprint(...)` raises `ESSLCertError` when `EVP_sha256` is unavailable
- `TryGetFingerprint(...)` returns `False` and clears output
