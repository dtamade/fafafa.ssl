# 2026-03-07 Integration Runtime Contract Consolidation

## Goal
Reduce script count by consolidating the three same-shape integration runtime contracts into `tests/scripts/test_integration_simple_runtime_contract.sh`, while keeping `tests/scripts/test_integration_runtime_contract.sh` untouched.

## Architecture
- Keep the custom-marker integration smoke contract (`test_integration_runtime_contract.sh`) separate.
- Fold the `RESULT: ALL TESTS PASSED` integration families into one script:
  - former simple batch
  - former primitives batch
  - former extended batch
- Delete the redundant scripts once the consolidated script proves green.

## Files
- Modify: `tests/scripts/test_integration_simple_runtime_contract.sh`
- Delete the previously separate primitive and extended integration runtime scripts.
- Add: `docs/plans/2026-03-07-integration-runtime-contract-consolidation.md`

## Steps
1. RED
- Assert the consolidated target is incomplete before change:
  - `rg -F 'tests/integration/test_buffer_simple.pas' tests/scripts/test_integration_simple_runtime_contract.sh`
  - `rg -F 'tests/integration/test_asn1_module.pas' tests/scripts/test_integration_simple_runtime_contract.sh`
- Expected: both commands fail because the simple contract does not yet cover the primitive/extended files.

2. GREEN
- Expand `test_integration_simple_runtime_contract.sh` to cover all `RESULT: ALL TESTS PASSED` integration programs.
- Delete the redundant primitive/extended scripts.
- Re-run:
  - `bash tests/scripts/test_integration_simple_runtime_contract.sh`

3. Regression
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- The consolidated simple runtime contract passes.
- Primitive and extended coverage now lives in a single script.
- Fast-local and module compilation remain green.
