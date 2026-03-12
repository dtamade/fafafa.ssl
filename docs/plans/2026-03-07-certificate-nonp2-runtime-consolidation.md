# 2026-03-07 Certificate Non-P2 Runtime Consolidation

## Goal
Reduce script count by consolidating certificate non-P2 runtime contract coverage into `tests/scripts/test_certificate_utilities_runtime_contract.sh`.

## Architecture
- Keep one certificate non-P2 runtime contract as the canonical entrypoint.
- Fold focused non-P2 runtime checks into `test_certificate_utilities_runtime_contract.sh`.
- Delete redundant focused scripts once the consolidated contract is green.
- Keep `test_cert_load_debug_contract.sh` untouched because it is compile-only / environment-specific.

## Files
- Modify: `tests/scripts/test_certificate_utilities_runtime_contract.sh`
- Delete the previously separate focused non-P2 certificate runtime scripts.
- Add: `docs/plans/2026-03-07-certificate-nonp2-runtime-consolidation.md`

## Steps
1. Baseline
- `bash tests/scripts/test_certificate_utilities_runtime_contract.sh`
- Expected: pass on the current smaller utility subset.

2. Consolidate
- Extend `test_certificate_utilities_runtime_contract.sh` to cover:
  - `test_certificate_unit.pas`
  - `test_certificate_real.pas`
  - `test_cert_utils_enterprise.pas`
  - `test_cert_utils_try.pas`
  - `test_p2_pkcs12_create_parse.pas`
  - `test_p2_pkcs7_encrypt_decrypt.pas`
  - `test_p2_pkcs7_sign_verify.pas`
  - `test_pkcs7_data_debug.pas`
  - `test_cert_store.pas`
- Delete the redundant focused non-P2 runtime scripts.

3. Regression
- `bash tests/scripts/test_certificate_utilities_runtime_contract.sh`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Consolidated utility runtime contract passes.
- Redundant focused non-P2 runtime scripts are no longer needed.
- Repository gates remain green.
