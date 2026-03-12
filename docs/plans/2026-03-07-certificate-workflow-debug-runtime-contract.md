# 2026-03-07 Certificate Workflow/Debug Runtime Contract

> Superseded on 2026-03-07 by `tests/scripts/test_certificate_utilities_runtime_contract.sh`, while `tests/scripts/test_cert_load_debug_contract.sh` remains the dedicated compile-only environment probe.

## Goal
Cover the remaining CI-safe certificate workflow/debug entrypoints with a focused runtime contract using stable ASCII completion markers, while keeping `test_cert_load_debug.pas` out of general runtime due its environment-specific filesystem assumptions.

## Architecture
- Cover four self-contained programs in the consolidated non-P2 certificate runtime contract:
  - `tests/certificate/test_p2_pkcs12_create_parse.pas`
  - `tests/certificate/test_p2_pkcs7_encrypt_decrypt.pas`
  - `tests/certificate/test_p2_pkcs7_sign_verify.pas`
  - `tests/certificate/test_pkcs7_data_debug.pas`
- Add a separate compile-only contract for `tests/certificate/test_cert_load_debug.pas`.
- Minimal program changes:
  - append one stable ASCII `[PASS] ... completed` marker on each successful path
  - do not change existing test logic or summaries

## Files
- Cover via: `tests/scripts/test_certificate_utilities_runtime_contract.sh`
- Add: `tests/scripts/test_cert_load_debug_contract.sh`
- Modify: `tests/certificate/test_p2_pkcs12_create_parse.pas`
- Modify: `tests/certificate/test_p2_pkcs7_encrypt_decrypt.pas`
- Modify: `tests/certificate/test_p2_pkcs7_sign_verify.pas`
- Modify: `tests/certificate/test_pkcs7_data_debug.pas`

## Steps
1. RED
- `bash tests/scripts/test_certificate_utilities_runtime_contract.sh`
- Expected: fail because the four programs do not yet emit the new ASCII completion markers.

2. GREEN
- Add one stable success marker per program:
  - `[PASS] p2 pkcs12 create/parse completed`
  - `[PASS] p2 pkcs7 encrypt/decrypt completed`
  - `[PASS] p2 pkcs7 sign/verify completed`
  - `[PASS] pkcs7 data debug completed`
- Re-run:
  - `bash tests/scripts/test_certificate_utilities_runtime_contract.sh`
  - `bash -n tests/scripts/test_cert_load_debug_contract.sh`
  - `bash tests/scripts/test_cert_load_debug_contract.sh`

3. Regression
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- The consolidated non-P2 certificate runtime contract passes for the workflow/debug programs.
- The compile-only `test_cert_load_debug` contract passes.
- Repository regression gates remain green.
