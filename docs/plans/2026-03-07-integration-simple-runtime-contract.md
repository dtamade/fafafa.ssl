# Integration simple runtime contract（2026-03-07）

## Goal

Add one grouped runtime contract for the self-contained simple integration programs under `tests/integration`, then restore the batch to green by fixing the existing `test_x509_simple.pas` failure at the test root cause.

## Architecture

- Batch compile and run the simple integration programs with `-Fu./src -Fu./tests/framework`.
- Use the existing `TSimpleTestRunner` summary token `RESULT: ALL TESTS PASSED` as the runtime success marker.
- Keep networked or environment-dependent integration programs out of this batch.
- Fix `test_x509_simple.pas` only if the new contract demonstrates a real runtime failure.

## Files

- Add: `tests/scripts/test_integration_simple_runtime_contract.sh`
- Modify: `tests/integration/test_x509_simple.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. Create grouped runtime contract for simple integration programs.
2. Run the contract and capture the RED failure.
3. Investigate `test_x509_simple.pas` root cause.
4. Apply the smallest safe fix.
5. Re-run the contract and targeted regressions.
6. Update planning and evidence files.

## Expected outputs

- A new grouped runtime contract covering the simple self-contained integration programs.
- `test_x509_simple.pas` passes again in the grouped runtime batch.
- `bash tests/scripts/test_integration_simple_runtime_contract.sh` passes.
- `bash scripts/run_minimal_ci_gate.sh --fast-local` stays green.
- `python3 scripts/compile_all_modules.py` stays green.
