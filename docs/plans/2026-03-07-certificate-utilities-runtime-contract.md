# 2026-03-07 Certificate Utilities Runtime Contract

## Goal
Add a focused grouped runtime contract for the CI-safe certificate utility/workflow programs that already run correctly under redirected stdin, while leaving broken or broader-scope certificate programs out of this batch.

## Architecture
- New grouped runtime contract under `tests/scripts/`.
- Compile with `-Fu./src` from the repository root.
- Reuse existing success substrings that are stable enough under redirected stdout/stderr:
  - `Test completed successfully!`
  - `Result: All tests passed!`
  - `ALL CERTIFICATE CHAIN TESTS PASSED!`
  - `Result: ALL TESTS PASSED!`
- Keep `tests/certificate/test_cert_store.pas` out of scope because it currently fails to compile (`GetNativeHandle`).

## Files
- Add: `tests/scripts/test_certificate_utilities_runtime_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps
1. RED/Probe
- Confirm these programs compile and run successfully under redirected stdin:
  - `tests/certificate/test_cert_utils_simple.pas`
  - `tests/certificate/test_cert_utils.pas`
  - `tests/certificate/test_certificate_chain_methods.pas`
  - `tests/certificate/test_cert_verification_failures.pas`
- Confirm `tests/certificate/test_cert_store.pas` remains out of scope because it does not compile.

2. GREEN
- Add the grouped runtime contract using the existing success substrings.
- Run:
  - `bash -n tests/scripts/test_certificate_utilities_runtime_contract.sh`
  - `bash tests/scripts/test_certificate_utilities_runtime_contract.sh`

3. Regression
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- The new focused certificate utilities batch passes.
- Redirected stdin remains sufficient for the two `ReadLn`-using utilities.
- Main repository gates stay green.
