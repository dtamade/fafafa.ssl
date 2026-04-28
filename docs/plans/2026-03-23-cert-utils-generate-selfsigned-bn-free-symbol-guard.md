# Certificate Utils GenerateSelfSigned BN_free Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSelfSigned(...)` preserve its existing exception-based contract when `BN_free` becomes unavailable on the default RSA key-generation cleanup path, instead of dereferencing a nil BIGNUM cleanup helper in `GenerateRSAKey(...)` after the RSA key path may already have allocated or transferred ownership of other resources.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSelfSigned(...)` and its Try wrappers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful RSA self-signed generation behavior when `BN_free` is available
- keep this batch limited to the next earliest remaining default-path helper:
  - `GenerateRSAKey(...)` -> `finally BN_free(LExp)`
- do not redesign `GenerateSigned(...)`, earlier RSA/BIGNUM/EVP guards, X509/signing helpers, PEM export, or broader certificate generation logic
- if local ownership/nil-state cleanup must be normalized so the new cleanup-failure path does not double-free `Result` or `LKey`, keep that normalization scoped to `GenerateRSAKey(...)`

## Task 1: RED - Reproduce the self-signed BIGNUM cleanup gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_bn_free_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads the baseline core/BIO/X509/PEM/EVP/RSA/BN support needed to warm the RSA self-signed path
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - temporarily clears the global `BN_free` symbol
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal BIGNUM cleanup guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(BN_free)` guard around `GenerateRSAKey(...)` final BIGNUM cleanup before `BN_free(LExp)`
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when RSA BIGNUM cleanup helpers are unavailable
  - successful RSA self-signed generation remains unchanged when `BN_free` is available
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `False`
  - earlier RSA/BIGNUM/EVP guard batches stay unchanged in intent
- If needed for safe cleanup on the new failure path:
  - normalize local `Result` / `LKey` nil-state after manual frees or ownership transfer so the `BN_free`-missing branch can release the right resource exactly once before raising

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_bn_free_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_bn_free_symbol_contract -FEtmp/cert_utils_generate_selfsigned_bn_free_symbol_contract -otmp/cert_utils_generate_selfsigned_bn_free_symbol_contract/test_cert_utils_generate_selfsigned_bn_free_symbol_contract tests/test_cert_utils_generate_selfsigned_bn_free_symbol_contract.pas && ./tmp/cert_utils_generate_selfsigned_bn_free_symbol_contract/test_cert_utils_generate_selfsigned_bn_free_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-23-cert-utils-generate-selfsigned-bn-free-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_bn_free_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed generation contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` when `BN_free` is unavailable on the default RSA path
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` return `False` and clear outputs
