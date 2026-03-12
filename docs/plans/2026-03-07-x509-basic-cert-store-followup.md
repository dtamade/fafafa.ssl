# 2026-03-07 X509 Basic + Cert Store Follow-up

> The focused runtime validations in this follow-up were consolidated on 2026-03-07 into `tests/scripts/test_integration_simple_runtime_contract.sh` and `tests/scripts/test_certificate_utilities_runtime_contract.sh`; keep this file as the historical record of the test-semantic fixes.

## Goal
Close two remaining high-signal red points discovered during contract expansion:
- `tests/integration/test_x509_basic.pas` runtime semantic failures
- `tests/certificate/test_cert_store.pas` stale native-handle API compile failure

## Architecture
- Contract-first:
  - extend `tests/scripts/test_integration_simple_runtime_contract.sh` to include `test_x509_basic.pas`
  - extend `tests/scripts/test_certificate_utilities_runtime_contract.sh` to include `test_cert_store.pas`
- Minimal fixes:
  - align `test_x509_basic.pas` with current OpenSSL name/value semantics and explicitly set certificate version before asserting retrieval
  - align `test_cert_store.pas` with the canonical native-handle helper API (`fafafa.ssl.native_handle`)

## Files
- Modify: `tests/integration/test_x509_basic.pas`
- Modify: `tests/certificate/test_cert_store.pas`
- Modify: `tests/scripts/test_integration_simple_runtime_contract.sh`
- Modify: `tests/scripts/test_certificate_utilities_runtime_contract.sh`

## Steps
1. RED
- `fpc -Fu./src -Fu./tests/framework tests/integration/test_x509_basic.pas -otmp/test_x509_basic_fix && ./tmp/test_x509_basic_fix`
- `fpc -Fu./src tests/certificate/test_cert_store.pas -otmp/test_cert_store_fix`
- Expected:
  - `test_x509_basic.pas` fails at bare-name/certificate-version assumptions
  - `test_cert_store.pas` fails on stale `GetNativeHandle` member access

2. GREEN
- Update `test_x509_basic.pas`:
  - use UTF-8 name-entry encoding/auto-length for the exercised fields
  - set certificate version in `TestX509BasicFields` before retrieving it
- Update `test_cert_store.pas`:
  - import `fafafa.ssl.native_handle`
  - use `IsNativeHandleAvailable(Store)` instead of stale member access
- Re-run focused commands and both grouped contracts.

3. Regression
- `bash tests/scripts/test_integration_simple_runtime_contract.sh`
- `bash tests/scripts/test_certificate_utilities_runtime_contract.sh`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- `test_x509_basic.pas` ends with `RESULT: ALL TESTS PASSED`
- `test_cert_store.pas` compiles/runs and ends with `All tests PASSED!`
- Both grouped contracts and repo gates remain green
