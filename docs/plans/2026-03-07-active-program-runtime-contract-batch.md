# 2026-03-07 Active Program Runtime Contract Batch

## Goal
Provide one stable top-level batch entrypoint that exercises all current active Pascal program runtime contracts added across integration, certificate, examples, and smoke suites.

## Architecture
- Add `tests/scripts/test_active_program_runtime_contract_batch.sh` as the manual super-batch runner.
- Add `tests/scripts/test_active_program_runtime_contract_batch_coverage_contract.sh` to pin the expected runtime-contract membership and prevent silent drift.
- Keep `scripts/run_minimal_ci_gate.sh --fast-local` unchanged so the daily fast gate stays cheap; the new batch is for broader local regression sweeps.

## Files
- Add: `tests/scripts/test_active_program_runtime_contract_batch.sh`
- Add: `tests/scripts/test_active_program_runtime_contract_batch_coverage_contract.sh`
- Add: `docs/plans/2026-03-07-active-program-runtime-contract-batch.md`

## Steps
1. RED
- Run the coverage contract before the batch exists.
- Expected: fail due to missing batch script.

2. GREEN
- Add the batch script with the full current active runtime-contract list.
- Add the coverage contract to verify completeness and no duplicates.
- Re-run:
  - `bash tests/scripts/test_active_program_runtime_contract_batch_coverage_contract.sh`
  - `bash tests/scripts/test_active_program_runtime_contract_batch.sh`

3. Regression
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Coverage contract passes.
- Active runtime contract batch passes.
- Minimal gate and module compilation remain green.
