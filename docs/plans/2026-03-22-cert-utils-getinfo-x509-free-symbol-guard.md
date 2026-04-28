# Certificate Utils GetInfo X509_free Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GetInfo(...)` preserve its existing best-effort silent-degrade contract when `X509_free` is unavailable, instead of dereferencing a nil X509 cleanup helper after info extraction has already completed.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GetInfo(...)` / `TryGetInfo(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- use a SAN-bearing certificate fixture so the code path definitely reaches full metadata extraction before certificate cleanup
- preserve current successful metadata extraction behavior when `X509_free` is available
- do not redesign `TryGetInfo(...)`, `BIO_free`, `VerifyChain(...)`, or broader info parsing behavior

## Task 1: RED - Reproduce the certificate cleanup symbol gap

**Files:**
- Add: `tests/test_cert_utils_getinfo_x509_free_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/certs/san-test.pem`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads a valid SAN-bearing certificate PEM fixture and warms a normal `GetInfo(...)` path
  - verifies the warmup fixture produces non-empty `SubjectAltNames`
  - temporarily clears `X509_free`
  - asserts direct `TCertificateUtils.GetInfo(...)` must not raise and must preserve the already extracted subject, issuer, version, `NotBefore`, `NotAfter`, serial number, signature algorithm, public-key type, public-key bits, `IsCA`, key-usage, and decoded `SubjectAltNames`
  - asserts `TCertificateUtils.TryGetInfo(...)` must not raise, must preserve the same full info, and must keep its `True` return value because `GetInfo(...)` no longer raises
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal certificate cleanup guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(X509_free)` guard inside `TCertificateUtils.GetInfo(...)` around the certificate cleanup path
- Preserve current behavior:
  - helper loss keeps the already extracted subject/public-key metadata plus `IsCA`, key-usage, and decoded `SubjectAltNames`
  - `SubjectAltNames` remains allocated and keeps the decoded SAN entries
  - outer BIO cleanup and other helper families stay untouched for separate isolated batches
  - `TryGetInfo(...)` remains non-throwing and still returns `True`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_getinfo_x509_free_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_getinfo_x509_free_symbol_contract -FEtmp/cert_utils_getinfo_x509_free_symbol_contract -otmp/cert_utils_getinfo_x509_free_symbol_contract/test_cert_utils_getinfo_x509_free_symbol_contract tests/test_cert_utils_getinfo_x509_free_symbol_contract.pas && ./tmp/cert_utils_getinfo_x509_free_symbol_contract/test_cert_utils_getinfo_x509_free_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-getinfo-x509-free-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_getinfo_x509_free_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused `GetInfo` contract passes without raw `EAccessViolation`
- missing `X509_free` preserves all already extracted metadata plus decoded `SubjectAltNames` instead of crashing during cleanup
- full module compile remains green
