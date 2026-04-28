# Certificate Utils GenerateSelfSigned BN_new Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSelfSigned(...)` preserve its existing exception-based contract when `BN_new` becomes unavailable on the default RSA key-generation path, instead of dereferencing a nil BIGNUM allocation helper after RSA allocation has already succeeded.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSelfSigned(...)` and its Try wrappers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful RSA self-signed generation behavior when `BN_new` is available
- keep this batch limited to the next earliest remaining default-path helper:
  - `GenerateRSAKey(...)` -> `BN_new()`
- do not redesign `GenerateSigned(...)`, later RSA keygen helpers (`BN_set_word`, `RSA_generate_key_ex`, `EVP_PKEY_new`, `EVP_PKEY_assign`, `BN_free`), X509/signing helpers, PEM export, or cleanup logic

## Task 1: RED - Reproduce the self-signed BIGNUM allocation gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_bn_new_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads the baseline core/BIO/X509/PEM/EVP/RSA/BN support needed to warm the RSA self-signed path
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - temporarily clears the global `BN_new` symbol
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal BIGNUM allocation guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(BN_new)` guard inside `TCertificateUtils.GenerateRSAKey(...)` before `LExp := BN_new()`
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when RSA exponent-allocation helpers are unavailable
  - successful RSA self-signed generation remains unchanged when `BN_new` is available
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `False`
  - later RSA keygen helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_bn_new_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_bn_new_symbol_contract -FEtmp/cert_utils_generate_selfsigned_bn_new_symbol_contract -otmp/cert_utils_generate_selfsigned_bn_new_symbol_contract/test_cert_utils_generate_selfsigned_bn_new_symbol_contract tests/test_cert_utils_generate_selfsigned_bn_new_symbol_contract.pas && ./tmp/cert_utils_generate_selfsigned_bn_new_symbol_contract/test_cert_utils_generate_selfsigned_bn_new_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-23-cert-utils-generate-selfsigned-bn-new-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_bn_new_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed generation contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` when `BN_new` is unavailable on the default RSA path
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` return `False` and clear outputs
