# Certificate Utils GenerateSelfSigned EC Keygen Family Plan

**Goal:** Close the `GenerateSelfSigned(...) -> GenerateECKey(...)` helper family so the ECDSA self-signed path preserves its controlled exception contract when EC key-generation helpers disappear, instead of dereferencing nil OpenSSL function pointers or double-freeing local state.

**Architecture:** Keep this batch narrow:

- add one focused family-level contract test around `GenerateSelfSigned(...)` and `TryGenerateSelfSigned(...)` on the ECDSA path
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful ECDSA self-signed generation behavior when the helpers remain available
- close the local `GenerateECKey(...)` family for the reachable helper set:
  - `OBJ_txt2nid(...)`
  - `EC_KEY_new_by_curve_name(...)`
  - `EC_KEY_generate_key(...)`
  - `EVP_PKEY_new(...)`
  - `EVP_PKEY_assign(...)`
  - `EVP_PKEY_free(...)`
  - `EC_KEY_free(...)`
- do not redesign RSA/Ed25519 key generation, `GenerateSigned(...)`, X509 mutation, or PEM export

## Task 1: RED - Reproduce the EC keygen family gaps

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_ec_keygen_family_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write one focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads the BIO/X509/X509v3/PEM/EVP/EC/OBJ support required by `GenerateSelfSigned(...)`
  - verifies a normal ECDSA self-signed path succeeds
  - covers these family scenarios:
    - direct entry helper missing: `OBJ_txt2nid`
    - delayed-loss after curve lookup: `EC_KEY_new_by_curve_name`
    - delayed-loss after EC key allocation: `EC_KEY_generate_key`
    - delayed-loss after EC key generation: `EVP_PKEY_new`
    - delayed-loss after EVP container allocation: `EVP_PKEY_assign`
    - delayed-loss on failed assign cleanup: `EVP_PKEY_free`
    - delayed-loss on except cleanup after failed assign: `EC_KEY_free`
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal EC keygen family guards

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Harden `TCertificateUtils.GenerateECKey(...)` at each actual dereference boundary:
  - `OBJ_txt2nid`
  - `EC_KEY_new_by_curve_name`
  - `EC_KEY_generate_key`
  - `EVP_PKEY_new`
  - `EVP_PKEY_assign`
  - `EVP_PKEY_free`
  - `EC_KEY_free`
- Normalize local ownership to avoid double-free on `EVP_PKEY_assign(...)` failure:
  - clear `Result` after freeing it
  - transfer ownership with `LKey := nil` once `EVP_PKEY_assign(...)` succeeds
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when the EC keygen family helpers disappear
  - `TryGenerateSelfSigned(...)` remains non-throwing and returns `False`
  - `TryGenerateSelfSignedSimple(...)` stays untouched because it exercises the default RSA path rather than ECDSA

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_ec_keygen_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_ec_keygen_family_contract -FEtmp/cert_utils_generate_selfsigned_ec_keygen_family_contract -otmp/cert_utils_generate_selfsigned_ec_keygen_family_contract/test_cert_utils_generate_selfsigned_ec_keygen_family_contract tests/test_cert_utils_generate_selfsigned_ec_keygen_family_contract.pas && ./tmp/cert_utils_generate_selfsigned_ec_keygen_family_contract/test_cert_utils_generate_selfsigned_ec_keygen_family_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-generate-selfsigned-ec-keygen-family.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_ec_keygen_family_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- the focused family-level contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` for each EC keygen family scenario
- `TryGenerateSelfSigned(...)` returns `False` and clears outputs for each EC keygen family scenario
- normal ECDSA self-signed generation still succeeds within the family test warmup/baseline path
