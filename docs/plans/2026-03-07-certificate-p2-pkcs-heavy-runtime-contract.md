# 2026-03-07 Certificate P2 PKCS Heavy Runtime Contract

## Goal
Extend the existing `tests/scripts/test_certificate_p2_pkcs_runtime_contract.sh` to cover the remaining heavy-but-green PKCS certificate programs with stable ASCII completion markers.

## Architecture
- Contract-first:
  - extend `tests/scripts/test_certificate_p2_pkcs_runtime_contract.sh`
  - require stable ASCII completion markers for:
    - `tests/certificate/test_p2_pkcs12_comprehensive.pas`
    - `tests/certificate/test_p2_pkcs7_boundary.pas`
    - `tests/certificate/test_p2_pkcs7_comprehensive.pas`
- Minimal program change:
  - add one `[PASS] ... completed` line in each success path
  - keep existing summary output and behavior unchanged

## Files
- Modify: `tests/scripts/test_certificate_p2_pkcs_runtime_contract.sh`
- Modify: `tests/certificate/test_p2_pkcs12_comprehensive.pas`
- Modify: `tests/certificate/test_p2_pkcs7_boundary.pas`
- Modify: `tests/certificate/test_p2_pkcs7_comprehensive.pas`

## Steps
1. RED
- Extend the contract with the 3 new programs and ASCII pass tokens.
- Run:
  - `bash tests/scripts/test_certificate_p2_pkcs_runtime_contract.sh`
- Expected: fail because the new programs do not yet print the ASCII completion markers.

2. GREEN
- Add the minimum ASCII `[PASS] ... completed` lines on the existing success paths.
- Re-run:
  - `bash tests/scripts/test_certificate_p2_pkcs_runtime_contract.sh`
- Expected: contract passes.

3. Regression
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Existing PKCS contract coverage stays green.
- The 3 newly covered heavy PKCS programs emit stable ASCII completion markers.
- Repository gates remain green.
