# Certificate Utils GenerateSelfSigned AddNameEntry X509_NAME_add_entry_by_txt Symbol Guard Plan

**Goal:** Make the `TCertificateUtils.GenerateSelfSigned(...)` execution path preserve its existing exception-based contract when `X509_NAME_add_entry_by_txt` is unavailable inside `AddNameEntry(...)`, instead of dereferencing a nil subject-name entry helper.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GenerateSelfSigned(...)` and its Try wrappers
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful self-signed generation behavior when `X509_NAME_add_entry_by_txt` is available
- do not redesign `GenerateSigned(...)`, `X509_get_subject_name`, `X509_set_issuer_name`, extension helpers, PEM export, or broader certificate generation logic

## Task 1: RED - Reproduce the self-signed subject-name entry gap

**Files:**
- Add: `tests/test_cert_utils_generate_selfsigned_addnameentry_x509_name_add_entry_by_txt_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - warms a normal RSA `GenerateSelfSigned(...)` path
  - temporarily clears `X509_NAME_add_entry_by_txt`
  - asserts direct `TCertificateUtils.GenerateSelfSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TCertificateUtils.TryGenerateSelfSigned(...)` and `TCertificateUtils.TryGenerateSelfSignedSimple(...)` must not raise, must return `False`, and must clear outputs
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal AddNameEntry subject-name entry guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(X509_NAME_add_entry_by_txt)` guard inside `TCertificateUtils.AddNameEntry(...)` before `X509_NAME_add_entry_by_txt(...)`
- Preserve current behavior:
  - direct `GenerateSelfSigned(...)` raises controlled `ESSLCertError` when subject-name entry helpers are unavailable
  - successful generation remains unchanged when `X509_NAME_add_entry_by_txt` is available
  - `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` remain non-throwing and return `False`
  - later generation helpers stay untouched for separate isolated batches

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_generate_selfsigned_addnameentry_x509_name_add_entry_by_txt_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_addnameentry_x509_name_add_entry_by_txt_symbol_contract -FEtmp/cert_utils_generate_selfsigned_addnameentry_x509_name_add_entry_by_txt_symbol_contract -otmp/cert_utils_generate_selfsigned_addnameentry_x509_name_add_entry_by_txt_symbol_contract/test_cert_utils_generate_selfsigned_addnameentry_x509_name_add_entry_by_txt_symbol_contract tests/test_cert_utils_generate_selfsigned_addnameentry_x509_name_add_entry_by_txt_symbol_contract.pas && ./tmp/cert_utils_generate_selfsigned_addnameentry_x509_name_add_entry_by_txt_symbol_contract/test_cert_utils_generate_selfsigned_addnameentry_x509_name_add_entry_by_txt_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-generate-selfsigned-addnameentry-x509-name-add-entry-by-txt-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_generate_selfsigned_addnameentry_x509_name_add_entry_by_txt_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused self-signed generation contract passes without raw `EAccessViolation`
- direct `GenerateSelfSigned(...)` raises `ESSLCertError` when `X509_NAME_add_entry_by_txt` is unavailable
- `TryGenerateSelfSigned(...)` and `TryGenerateSelfSignedSimple(...)` return `False` and clear outputs
