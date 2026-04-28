# Certificate Utils GenerateSelfSigned ASN1_INTEGER_set Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GenerateSelfSigned(...)` preserve its existing exception-based contract when `ASN1_INTEGER_set` is unavailable, instead of dereferencing a nil ASN.1 serial setter.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSelfSigned(...)` and its Try wrappers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful self-signed generation behavior when `ASN1_INTEGER_set` is available
- do not redesign `GenerateSigned(...)`, `X509_get_notBefore` / `X509_get_notAfter`, `X509_gmtime_adj`, subject-name helpers, PEM export, or broader certificate generation logic

## Task 1: RED - Reproduce the self-signed serial-setter gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_asn1_integer_set_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - explicitly loads ASN.1 support for `ASN1_INTEGER_set`
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - temporarily clears `ASN1_INTEGER_set`
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal self-signed serial-setter guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(ASN1_INTEGER_set)` guard inside `TCertificateUtils.GenerateSelfSigned(...)` after `LSerial := X509_get_serialNumber(LCert)` and before any `ASN1_INTEGER_set(...)` call
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when ASN.1 serial setters are unavailable
  - successful generation remains unchanged when `ASN1_INTEGER_set` is available
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `False`
  - later generation helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_asn1_integer_set_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_asn1_integer_set_symbol_contract -FEtmp/cert_utils_generate_selfsigned_asn1_integer_set_symbol_contract -otmp/cert_utils_generate_selfsigned_asn1_integer_set_symbol_contract/test_cert_utils_generate_selfsigned_asn1_integer_set_symbol_contract tests/test_cert_utils_generate_selfsigned_asn1_integer_set_symbol_contract.pas && ./tmp/cert_utils_generate_selfsigned_asn1_integer_set_symbol_contract/test_cert_utils_generate_selfsigned_asn1_integer_set_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-generate-selfsigned-asn1-integer-set-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_asn1_integer_set_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed generation contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` when `ASN1_INTEGER_set` is unavailable
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` return `False` and clear outputs
