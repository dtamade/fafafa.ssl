# 2026-03-07 Integration Extended Runtime Contract

> Superseded on 2026-03-07 by `tests/scripts/test_integration_simple_runtime_contract.sh` and `docs/plans/2026-03-07-integration-runtime-contract-consolidation.md`.

## Goal
Cover the remaining CI-safe self-contained integration entrypoints that already compile and pass locally, without changing production code or test semantics.

## Architecture
- Add one grouped runtime contract for the next uncovered integration batch.
- Reuse the existing framework-aware compile path: `-Fu./src -Fu./tests/framework`.
- Reuse the existing stable summary token already emitted by these programs: `RESULT: ALL TESTS PASSED`.

## Files
- Cover via: `tests/scripts/test_integration_simple_runtime_contract.sh`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Steps
1. RED/Probe
- Compile and run each candidate individually:
  - `tests/integration/test_asn1_module.pas`
  - `tests/integration/test_ec_comprehensive.pas`
  - `tests/integration/test_error_recovery.pas`
  - `tests/integration/test_rsa_comprehensive.pas`
  - `tests/integration/test_rsa_integration.pas`
- Expected: identify only files that are local-runtime safe and already emit `RESULT: ALL TESTS PASSED`.

2. GREEN
- Add the five passing programs to `tests/scripts/test_integration_simple_runtime_contract.sh`.
- Verify with:
  - `bash tests/scripts/test_integration_simple_runtime_contract.sh`

3. Regression
- `bash tests/scripts/test_integration_simple_runtime_contract.sh`
- `bash tests/scripts/test_integration_simple_runtime_contract.sh`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- The new grouped contract passes with no Pascal source changes.
- Existing integration runtime batches and repo gates remain green.
