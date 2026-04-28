# Certificate Utils GenerateSelfSigned Ed25519 Keygen Family Plan

**Goal:** Close the `GenerateSelfSigned(...) -> GenerateEd25519Key(...)` helper family so the Ed25519 self-signed path preserves its controlled exception contract when key-generation helpers disappear after the entry helper gate, instead of dereferencing nil OpenSSL function pointers.

**Architecture:** Keep this batch narrow:

- add one focused family-level contract test around `GenerateSelfSigned(...)` and `TryGenerateSelfSigned(...)` on the Ed25519 path
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful Ed25519 self-signed generation behavior when the helpers remain available
- close only the reachable delayed-loss helpers inside `GenerateEd25519Key(...)`:
  - `EVP_PKEY_keygen_init(...)`
  - `EVP_PKEY_keygen(...)`
  - `EVP_PKEY_CTX_free(...)`
- do not redesign RSA/ECDSA key generation, `GenerateSigned(...)`, PEM export, or unrelated cleanup paths

## Task 1: RED - Reproduce the Ed25519 keygen family gaps

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_ed25519_keygen_family_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write one focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads the BIO/X509/X509v3/PEM/EVP support required by `GenerateSelfSigned(...)`
  - warms a normal Ed25519 `GenerateSelfSigned(...)` path
  - uses delayed-loss wrappers to clear helpers only after the entry helper gate and preceding local step succeed:
    - clear `EVP_PKEY_keygen_init` immediately after `EVP_PKEY_CTX_new_id(...)`
    - clear `EVP_PKEY_keygen` immediately after `EVP_PKEY_keygen_init(...)`
    - clear `EVP_PKEY_CTX_free` immediately after `EVP_PKEY_keygen(...)`
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal Ed25519 keygen family guards

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add local delayed-loss guards inside `TCertificateUtils.GenerateEd25519Key(...)`:
  - require `Assigned(EVP_PKEY_keygen_init)` immediately before `EVP_PKEY_keygen_init(LCtx)`
  - require `Assigned(EVP_PKEY_keygen)` immediately before `EVP_PKEY_keygen(LCtx, LKey)`
  - guard `EVP_PKEY_CTX_free(LCtx)` in the `finally` block before dereference
- Normalize local ownership so the generated `PEVP_PKEY` does not leak when context cleanup becomes unavailable after successful key generation.
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when the Ed25519 keygen family helpers disappear after helper-gate success
  - `TryGenerateSelfSigned(...)` remains non-throwing and returns `False`
  - successful Ed25519 self-signed generation remains unchanged when the helpers are available
  - `TryGenerateSelfSignedSimple(...)` stays untouched because it exercises the default RSA path rather than Ed25519

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_ed25519_keygen_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_ed25519_keygen_family_contract -FEtmp/cert_utils_generate_selfsigned_ed25519_keygen_family_contract -otmp/cert_utils_generate_selfsigned_ed25519_keygen_family_contract/test_cert_utils_generate_selfsigned_ed25519_keygen_family_contract tests/test_cert_utils_generate_selfsigned_ed25519_keygen_family_contract.pas && ./tmp/cert_utils_generate_selfsigned_ed25519_keygen_family_contract/test_cert_utils_generate_selfsigned_ed25519_keygen_family_contract`
- `mkdir -p tmp/cert_utils_ed25519_contract && fpc -B -Fu./src -FUtmp/cert_utils_ed25519_contract -FEtmp/cert_utils_ed25519_contract -otmp/cert_utils_ed25519_contract/test_cert_utils_ed25519_contract tests/test_cert_utils_ed25519_contract.pas && ./tmp/cert_utils_ed25519_contract/test_cert_utils_ed25519_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-generate-selfsigned-ed25519-keygen-family.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_ed25519_keygen_family_contract.pas tests/test_cert_utils_ed25519_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- the focused family-level contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` for each delayed-loss Ed25519 keygen helper scenario
- `TryGenerateSelfSigned(...)` returns `False` and clears outputs for each delayed-loss scenario
- the existing Ed25519 baseline contract still passes on both self-signed and CA-signed leaf flows
