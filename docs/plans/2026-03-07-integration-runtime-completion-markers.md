# 2026-03-07 Integration Runtime Completion Markers

## Goal
Add a grouped runtime contract for currently runnable integration smoke programs and standardize stable ASCII completion markers so local CI can verify them robustly.

## Architecture
- Shell contract under `tests/scripts/`
- Pascal integration smoke programs under `tests/integration/`
- Uses existing `TSimpleTestRunner` summary flow and appends a final completion marker only on success

## Files
- `tests/scripts/test_integration_runtime_contract.sh`
- `tests/integration/test_bn_simple.pas`
- `tests/integration/test_asn1_simple.pas`
- `tests/integration/test_bio_simple.pas`
- `tests/integration/test_e2e_scenarios.pas`
- `tests/integration/test_integration_tls_end_to_end.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. Create grouped runtime contract that compiles/runs the five integration programs with `-Fu./tests/framework` and expects stable `[PASS] ... completed` markers.
2. Run the contract and capture the failing RED state.
3. Add minimal final success markers to each Pascal program without changing test semantics.
4. Re-run the new contract to reach GREEN.
5. Run focused regression plus repository gates: `bash tests/scripts/test_integration_pkcs11_runtime_contract.sh`, `bash scripts/run_minimal_ci_gate.sh --fast-local`, `python3 scripts/compile_all_modules.py`.
6. Update planning/memory files with evidence.

## Expected Outputs
- New grouped contract passes on Linux.
- Five integration programs print stable completion markers only on success.
- Existing integration and repo gates remain green.
