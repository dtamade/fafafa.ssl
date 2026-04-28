# Certificate Utils GenerateSelfSigned X509_get_serialNumber Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSelfSigned(...)` preserve its existing exception-based contract when `X509_get_serialNumber` is unavailable, instead of dereferencing a nil certificate serial helper.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSelfSigned(...)` and its Try wrappers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful self-signed generation behavior when `X509_get_serialNumber` is available
- do not redesign `GenerateSigned(...)`, allocation/version helpers, `ASN1_INTEGER_set`, validity helpers, PEM export, or broader certificate generation logic

## Task 1: RED - Reproduce the self-signed serial-helper gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_x509_get_serialnumber_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - temporarily clears `X509_get_serialNumber`
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal self-signed serial-helper guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(X509_get_serialNumber)` guard inside `TCertificateUtils.GenerateSelfSigned(...)` before `LSerial := X509_get_serialNumber(LCert)`
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when certificate serial helpers are unavailable
  - successful generation remains unchanged when `X509_get_serialNumber` is available
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `False`
  - later generation helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_x509_get_serialnumber_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_x509_get_serialnumber_symbol_contract -FEtmp/cert_utils_generate_selfsigned_x509_get_serialnumber_symbol_contract -otmp/cert_utils_generate_selfsigned_x509_get_serialnumber_symbol_contract/test_cert_utils_generate_selfsigned_x509_get_serialnumber_symbol_contract tests/test_cert_utils_generate_selfsigned_x509_get_serialnumber_symbol_contract.pas && ./tmp/cert_utils_generate_selfsigned_x509_get_serialnumber_symbol_contract/test_cert_utils_generate_selfsigned_x509_get_serialnumber_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-generate-selfsigned-x509-get-serialnumber-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_x509_get_serialnumber_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed generation contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` when `X509_get_serialNumber` is unavailable
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` return `False` and clear outputs
