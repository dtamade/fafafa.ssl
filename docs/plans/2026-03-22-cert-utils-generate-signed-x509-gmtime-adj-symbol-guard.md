# Certificate Utils GenerateSigned X509_gmtime_adj Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSigned(...)` preserve its existing exception-based contract when `X509_gmtime_adj` is unavailable on the leaf certificate validity-adjustment path, instead of dereferencing a nil validity-adjustment helper after both validity getters have already succeeded.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSigned(...)` and `TryGenerateSigned(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful CA-signed certificate generation behavior when `X509_gmtime_adj` is available
- do not redesign `GenerateSelfSigned(...)`, `X509_set_pubkey`, subject/issuer helpers, PEM export, or broader certificate generation logic

## Task 1: RED - Reproduce the signed leaf validity-adjustment gap

**Files:**
- Add: `tests/test_cert_utils_generate_signed_x509_gmtime_adj_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads BIO/PEM/X509/EVP support required by `GenerateSigned(...)`
  - warms a normal CA generation + `GenerateSigned(...)` path
  - temporarily clears `X509_gmtime_adj`
  - asserts direct `TCertificateUtils.GenerateSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSigned(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal signed leaf validity-adjustment guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(X509_gmtime_adj)` guard inside `TCertificateUtils.GenerateSigned(...)` before the first `X509_gmtime_adj(...)` call
- Preserve current behavior:
  - direct `GenerateSigned(...)` raises controlled `ESSLCertError` when leaf certificate validity-adjustment helpers are unavailable
  - successful generation remains unchanged when `X509_gmtime_adj` is available
  - `TryGenerateSigned(...)` remains non-throwing and returns `False`
  - later `X509_set_pubkey`, subject/issuer helpers, extension chain, PEM export, and cleanup helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_signed_x509_gmtime_adj_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_x509_gmtime_adj_symbol_contract -FEtmp/cert_utils_generate_signed_x509_gmtime_adj_symbol_contract -otmp/cert_utils_generate_signed_x509_gmtime_adj_symbol_contract/test_cert_utils_generate_signed_x509_gmtime_adj_symbol_contract tests/test_cert_utils_generate_signed_x509_gmtime_adj_symbol_contract.pas && ./tmp/cert_utils_generate_signed_x509_gmtime_adj_symbol_contract/test_cert_utils_generate_signed_x509_gmtime_adj_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-generate-signed-x509-gmtime-adj-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_signed_x509_gmtime_adj_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused signed-generation contract passes without raw `EAccessViolation`
- direct `GenerateSigned(...)` raises `ESSLCertError` when `X509_gmtime_adj` is unavailable on the leaf validity-adjustment path
- `TryGenerateSigned(...)` returns `False` and clears outputs
