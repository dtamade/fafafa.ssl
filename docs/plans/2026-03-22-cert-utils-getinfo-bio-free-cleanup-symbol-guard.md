# Certificate Utils GetInfo BIO_free Cleanup Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GetInfo(...)` preserve its existing best-effort silent-degrade contract when `BIO_free` becomes unavailable during outer cleanup, instead of dereferencing a nil BIO cleanup helper after info extraction has already completed.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GetInfo(...)` / `TryGetInfo(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- use a SAN-bearing certificate fixture so the code path definitely reaches full metadata extraction before outer BIO cleanup
- pass the existing entry guard first, then disable `BIO_free` after certificate cleanup via a stub so this batch covers outer cleanup rather than the earlier empty-info entry contract
- preserve current successful metadata extraction behavior when `BIO_free` remains available
- do not redesign `TryGetInfo(...)`, `HasCertificatePEMReadBIOHelpers`, `VerifyChain(...)`, or broader info parsing behavior

## Task 1: RED - Reproduce the outer BIO cleanup symbol gap

**Files:**
- Add: `tests/test_cert_utils_getinfo_bio_free_cleanup_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/certs/san-test.pem`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads a valid SAN-bearing certificate PEM fixture and warms a normal `GetInfo(...)` path
  - verifies the warmup fixture produces non-empty `SubjectAltNames`
  - temporarily replaces `X509_free` with a stub that completes certificate cleanup and then disables `BIO_free`, re-arming it before each `GetInfo(...)` / `TryGetInfo(...)` invocation so the entry guard still passes
  - asserts direct `TCertificateUtils.GetInfo(...)` must not raise and must preserve the already extracted subject, issuer, version, `NotBefore`, `NotAfter`, serial number, signature algorithm, public-key type, public-key bits, `IsCA`, key-usage, and decoded `SubjectAltNames`
  - asserts `TCertificateUtils.TryGetInfo(...)` must not raise, must preserve the same full info, and must keep its `True` return value because `GetInfo(...)` no longer raises
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal outer BIO cleanup guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(BIO_free)` guard inside `TCertificateUtils.GetInfo(...)` around the outer BIO cleanup path
- Preserve current behavior:
  - helper loss after entry keeps the already extracted subject/public-key metadata plus `IsCA`, key-usage, and decoded `SubjectAltNames`
  - `SubjectAltNames` remains allocated and keeps the decoded SAN entries
  - earlier `HasCertificatePEMReadBIOHelpers` entry behavior stays untouched for separate coverage already in place
  - `TryGetInfo(...)` remains non-throwing and still returns `True`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_getinfo_bio_free_cleanup_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_getinfo_bio_free_cleanup_symbol_contract -FEtmp/cert_utils_getinfo_bio_free_cleanup_symbol_contract -otmp/cert_utils_getinfo_bio_free_cleanup_symbol_contract/test_cert_utils_getinfo_bio_free_cleanup_symbol_contract tests/test_cert_utils_getinfo_bio_free_cleanup_symbol_contract.pas && ./tmp/cert_utils_getinfo_bio_free_cleanup_symbol_contract/test_cert_utils_getinfo_bio_free_cleanup_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-getinfo-bio-free-cleanup-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_getinfo_bio_free_cleanup_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused `GetInfo` contract passes without raw `EAccessViolation`
- missing `BIO_free` during outer cleanup preserves all already extracted metadata plus decoded `SubjectAltNames` instead of crashing after a successful parse
- full module compile remains green
