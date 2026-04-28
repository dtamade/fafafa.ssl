# Certificate Utils GetInfo X509NameToString X509_NAME_print_ex Symbol Guard Plan

**Goal:** Make `TCertificateUtils.GetInfo(...)` preserve its best-effort partial-info contract when `X509_NAME_print_ex` is unavailable inside `X509NameToString(...)`, instead of dereferencing a nil name-print helper while subject/issuer string conversion is running.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `GetInfo(...)` / `TryGetInfo(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- use a SAN-bearing certificate fixture so the code path proves later metadata and SAN decoding still survive name-string helper loss
- treat this as a local helper gap in `X509NameToString(...)`, not as a broader `GetInfo(...)` entry/helper-family redesign
- preserve current successful metadata extraction behavior when `X509_NAME_print_ex` is available
- do not redesign `TryGetInfo(...)`, `BIO_s_mem`, `BIO_new`, `BIO_read`, `VerifyChain(...)`, or broader info parsing behavior

## Task 1: RED - Reproduce the X509NameToString X509_NAME_print_ex symbol gap

**Files:**
- Add: `tests/test_cert_utils_getinfo_x509_name_to_string_x509_name_print_ex_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/certs/san-test.pem`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads a valid SAN-bearing certificate PEM fixture and warms a normal `GetInfo(...)` path
  - verifies the warmup fixture produces non-empty `Subject`, `Issuer`, and `SubjectAltNames`
  - temporarily clears `X509_NAME_print_ex`
  - asserts direct `TCertificateUtils.GetInfo(...)` must not raise
  - asserts subject/issuer string fields degrade to empty because `X509NameToString(...)` cannot print the X509 name into its memory BIO
  - asserts later metadata (`Version`, `NotBefore`, `NotAfter`, `SerialNumber`, `SignatureAlgorithm`, `PublicKeyType`, `PublicKeyBits`, `IsCA`, `KeyUsage`) plus decoded `SubjectAltNames` are preserved
  - asserts `TCertificateUtils.TryGetInfo(...)` must not raise, must preserve the same partial/full info, and must keep its `True` return value because `GetInfo(...)` no longer raises
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal X509NameToString X509_NAME_print_ex guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(X509_NAME_print_ex)` guard inside `TCertificateUtils.X509NameToString(...)` before `X509_NAME_print_ex(LBIO, AName, 0, 0)`
- Preserve current behavior:
  - helper loss keeps subject/issuer string conversion at empty string
  - later `GetInfo(...)` metadata extraction and SAN decoding continue
  - `SubjectAltNames` remains allocated and keeps decoded SAN entries
  - `TryGetInfo(...)` remains non-throwing and returns `True`
  - the remaining helper gap inside `X509NameToString(...)` (`BIO_read`) stays untouched for a separate isolated batch

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_getinfo_x509_name_to_string_x509_name_print_ex_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_getinfo_x509_name_to_string_x509_name_print_ex_symbol_contract -FEtmp/cert_utils_getinfo_x509_name_to_string_x509_name_print_ex_symbol_contract -otmp/cert_utils_getinfo_x509_name_to_string_x509_name_print_ex_symbol_contract/test_cert_utils_getinfo_x509_name_to_string_x509_name_print_ex_symbol_contract tests/test_cert_utils_getinfo_x509_name_to_string_x509_name_print_ex_symbol_contract.pas && ./tmp/cert_utils_getinfo_x509_name_to_string_x509_name_print_ex_symbol_contract/test_cert_utils_getinfo_x509_name_to_string_x509_name_print_ex_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-22-cert-utils-getinfo-x509-name-to-string-x509-name-print-ex-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_getinfo_x509_name_to_string_x509_name_print_ex_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused `GetInfo` contract passes without raw `EAccessViolation`
- missing `X509_NAME_print_ex` inside `X509NameToString(...)` empties subject/issuer string conversion without losing later metadata or decoded `SubjectAltNames`
- full module compile remains green
