# 2026-03-07 Integration Primitives Runtime Contract

> Superseded on 2026-03-07 by `tests/scripts/test_integration_simple_runtime_contract.sh` and `docs/plans/2026-03-07-integration-runtime-contract-consolidation.md`.

## Goal
Add a focused grouped runtime contract for the remaining self-contained integration primitive programs that already succeed under `TSimpleTestRunner` and do not require network or fixture setup.

## Architecture
- New grouped runtime contract under `tests/scripts/`.
- Compile with `-Fu./src -Fu./tests/framework`.
- Use the existing summary token `RESULT: ALL TESTS PASSED` as the success marker.
- Keep known failing / higher-risk entries such as `tests/integration/test_x509_basic.pas` out of scope for this batch.

## Files
- Cover via: `tests/scripts/test_integration_simple_runtime_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps
1. RED/Probe
- Confirm candidate programs compile and run locally:
  - `tests/integration/test_buffer_simple.pas`
  - `tests/integration/test_dsa_simple.pas`
  - `tests/integration/test_ec_simple.pas`
  - `tests/integration/test_ecdsa_simple.pas`
- Confirm they emit `RESULT: ALL TESTS PASSED`.

2. GREEN
- Add the grouped runtime contract using the existing marker.
- Run:
  - `bash tests/scripts/test_integration_simple_runtime_contract.sh`

3. Regression
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- The new focused integration primitives batch passes.
- No production Pascal changes are required for this batch.
- Main repository gates stay green.
