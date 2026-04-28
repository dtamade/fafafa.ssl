# Certificate Utils GetInfo OPENSSL_sk_num Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GetInfo(...)` preserve its existing best-effort silent-degrade contract when `OPENSSL_sk_num` is unavailable, instead of dereferencing a nil SAN stack helper.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GetInfo(...)` / `TryGetInfo(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- use a SAN-bearing certificate fixture so the code path definitely reaches SAN stack traversal
- preserve current successful metadata extraction behavior when `OPENSSL_sk_num` is available
- do not redesign `TryGetInfo(...)`, `OPENSSL_sk_value`, `GENERAL_NAME_get0_value`, `GENERAL_NAMES_free`, `VerifyChain(...)`, or broader info parsing behavior

## Task 1: RED - Reproduce the SAN stack-count symbol gap

**Files:**
- Add: `tests/test_cert_utils_getinfo_openssl_sk_num_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/certs/san-test.pem`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads a valid SAN-bearing certificate PEM fixture and warms a normal `GetInfo(...)` path
  - verifies the warmup fixture produces non-empty `SubjectAltNames`
  - temporarily clears `OPENSSL_sk_num`
  - asserts direct `TCertificateUtils.GetInfo(...)` must not raise and must preserve the already extracted subject, issuer, version, `NotBefore`, `NotAfter`, serial number, signature algorithm, public-key type, public-key bits, `IsCA`, and key-usage while keeping `SubjectAltNames` allocated and empty because SAN traversal cannot start
  - asserts `TCertificateUtils.TryGetInfo(...)` must not raise, must preserve the same partial info, and must keep its `True` return value because `GetInfo(...)` no longer raises
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal SAN stack-count guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(OPENSSL_sk_num)` guard inside `TCertificateUtils.GetInfo(...)` after `X509_get_ext_d2i(...)` returns a non-nil SAN stack and before calling `OPENSSL_sk_num`
- Preserve current behavior:
  - helper loss keeps the already extracted subject/public-key metadata plus `IsCA` and key-usage
  - `SubjectAltNames` remains allocated and empty
  - later SAN helpers stay untouched for separate isolated batches
  - `TryGetInfo(...)` remains non-throwing and still returns `True`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_getinfo_openssl_sk_num_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_getinfo_openssl_sk_num_symbol_contract -FEtmp/cert_utils_getinfo_openssl_sk_num_symbol_contract -otmp/cert_utils_getinfo_openssl_sk_num_symbol_contract/test_cert_utils_getinfo_openssl_sk_num_symbol_contract tests/test_cert_utils_getinfo_openssl_sk_num_symbol_contract.pas && ./tmp/cert_utils_getinfo_openssl_sk_num_symbol_contract/test_cert_utils_getinfo_openssl_sk_num_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-getinfo-openssl-sk-num-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_getinfo_openssl_sk_num_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused `GetInfo` contract passes without raw `EAccessViolation`
- missing `OPENSSL_sk_num` preserves already extracted metadata while leaving SAN output empty instead of crashing
- full module compile remains green
