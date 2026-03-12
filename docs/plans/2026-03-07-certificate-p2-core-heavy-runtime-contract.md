# 2026-03-07 Certificate P2 Core Heavy Runtime Contract

## Goal
Extend `tests/scripts/test_certificate_p2_core_runtime_contract.sh` to cover the remaining heavy-but-green core P2 certificate programs with stable ASCII completion markers.

## Architecture
- Contract-first:
  - extend the existing grouped runtime contract with 5 more programs
  - require explicit stable ASCII markers for each new program
- Minimal code change:
  - add one `[PASS] ... completed` line in each program's success path
  - keep existing summary/output semantics unchanged

## Files
- Modify: `tests/scripts/test_certificate_p2_core_runtime_contract.sh`
- Modify: `tests/certificate/test_p2_cms_boundary.pas`
- Modify: `tests/certificate/test_p2_cms_comprehensive.pas`
- Modify: `tests/certificate/test_p2_ct_comprehensive.pas`
- Modify: `tests/certificate/test_p2_ocsp_comprehensive.pas`
- Modify: `tests/certificate/test_p2_ts_comprehensive.pas`

## Steps
1. RED
- Extend the contract to require new ASCII markers.
- Run `bash tests/scripts/test_certificate_p2_core_runtime_contract.sh`.
- Expected: fail because the new markers do not exist yet.

2. GREEN
- Add one stable `[PASS] ... completed` line to each success path.
- Re-run `bash tests/scripts/test_certificate_p2_core_runtime_contract.sh`.
- Expected: pass.

3. Regression
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Extended core P2 contract passes.
- Existing covered programs remain green.
- New heavy core programs emit stable ASCII markers.
