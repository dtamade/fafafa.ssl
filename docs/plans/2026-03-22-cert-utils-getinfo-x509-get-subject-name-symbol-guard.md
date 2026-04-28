# Certificate Utils GetInfo X509_get_subject_name Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GetInfo(...)` preserve its existing silent-degrade contract when `X509_get_subject_name` is unavailable, instead of dereferencing a nil X509 metadata helper.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GetInfo(...)` / `TryGetInfo(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful metadata extraction behavior when `X509_get_subject_name` is available
- do not redesign `TryGetInfo(...)`, `X509_get_issuer_name`, `X509_get_version`, `VerifyChain(...)`, or broader info parsing behavior

## Task 1: RED - Reproduce the subject-name symbol gap

**Files:**
- Add: `tests/test_cert_utils_getinfo_x509_get_subject_name_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/certificate/test_certs/signer_cert.pem`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads a valid certificate PEM fixture and warms a normal `GetInfo(...)` path
  - temporarily clears `X509_get_subject_name`
  - asserts direct `TCertificateUtils.GetInfo(...)` must not raise and must degrade to empty info with allocated `SubjectAltNames`
  - asserts `TCertificateUtils.TryGetInfo(...)` must not raise and must return sanitized info
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal subject-name guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(X509_get_subject_name)` guard inside `TCertificateUtils.GetInfo(...)` after PEM parsing succeeds and before the first metadata extraction call
- Preserve current behavior:
  - helper loss still returns an empty `TCertInfo`
  - `SubjectAltNames` remains allocated
  - later metadata helpers stay untouched for separate isolated batches
  - `TryGetInfo(...)` remains non-throwing

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_getinfo_x509_get_subject_name_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_getinfo_x509_get_subject_name_symbol_contract -FEtmp/cert_utils_getinfo_x509_get_subject_name_symbol_contract -otmp/cert_utils_getinfo_x509_get_subject_name_symbol_contract/test_cert_utils_getinfo_x509_get_subject_name_symbol_contract tests/test_cert_utils_getinfo_x509_get_subject_name_symbol_contract.pas && ./tmp/cert_utils_getinfo_x509_get_subject_name_symbol_contract/test_cert_utils_getinfo_x509_get_subject_name_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-getinfo-x509-get-subject-name-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_getinfo_x509_get_subject_name_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused `GetInfo` contract passes without raw `EAccessViolation`
- missing `X509_get_subject_name` degrades to empty info instead of crashing
- full module compile remains green
