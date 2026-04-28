# Certificate Utils GetInfo EVP_PKEY_free Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GetInfo(...)` preserve its existing best-effort silent-degrade contract when `EVP_PKEY_free` is unavailable, instead of dereferencing a nil EVP cleanup helper during public-key extraction cleanup.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GetInfo(...)` / `TryGetInfo(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful metadata extraction behavior when `EVP_PKEY_free` is available
- do not redesign `TryGetInfo(...)`, `X509_check_ca`, `X509_get_key_usage`, SAN extraction, `VerifyChain(...)`, or broader info parsing behavior

## Task 1: RED - Reproduce the public-key cleanup symbol gap

**Files:**
- Add: `tests/test_cert_utils_getinfo_evp_pkey_free_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/certificate/test_certs/signer_cert.pem`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads a valid certificate PEM fixture and warms a normal `GetInfo(...)` path
  - temporarily clears `EVP_PKEY_free`
  - asserts direct `TCertificateUtils.GetInfo(...)` must not raise and must preserve the already extracted subject, issuer, version, `NotBefore`, `NotAfter`, serial number, signature algorithm, public-key type, and public-key bits while keeping `SubjectAltNames` allocated
  - asserts `TCertificateUtils.TryGetInfo(...)` must not raise, must preserve the same partial info, and must keep its `True` return value because `GetInfo(...)` no longer raises
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal cleanup guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(EVP_PKEY_free)` guard inside `TCertificateUtils.GetInfo(...)` around the `LPubKey` cleanup path
- Preserve current behavior:
  - helper loss keeps the already extracted `Subject`, `Issuer`, `Version`, `NotBefore`, `NotAfter`, `SerialNumber`, `SignatureAlgorithm`, `PublicKeyType`, and `PublicKeyBits`
  - `SubjectAltNames` remains allocated
  - later SAN and metadata helpers stay untouched and remain out of scope for this isolated cleanup batch
  - `TryGetInfo(...)` remains non-throwing and still returns `True`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_getinfo_evp_pkey_free_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_getinfo_evp_pkey_free_symbol_contract -FEtmp/cert_utils_getinfo_evp_pkey_free_symbol_contract -otmp/cert_utils_getinfo_evp_pkey_free_symbol_contract/test_cert_utils_getinfo_evp_pkey_free_symbol_contract tests/test_cert_utils_getinfo_evp_pkey_free_symbol_contract.pas && ./tmp/cert_utils_getinfo_evp_pkey_free_symbol_contract/test_cert_utils_getinfo_evp_pkey_free_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-getinfo-evp-pkey-free-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_getinfo_evp_pkey_free_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused `GetInfo` contract passes without raw `EAccessViolation`
- missing `EVP_PKEY_free` preserves `Subject`, `Issuer`, `Version`, `NotBefore`, `NotAfter`, `SerialNumber`, `SignatureAlgorithm`, `PublicKeyType`, and `PublicKeyBits` instead of crashing
- full module compile remains green
