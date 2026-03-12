# 2026-03-07 Certificate P2 Core Runtime Contract

## Goal
Cover the CI-safe P2 certificate core entrypoints that already compile and pass locally:
- `tests/certificate/test_p2_cms.pas`
- `tests/certificate/test_p2_ct.pas`
- `tests/certificate/test_p2_ocsp.pas`
- `tests/certificate/test_p2_ts.pas`

## Architecture
- Add one grouped runtime contract using the existing `fpc -Fu./src` compile path.
- Reuse the existing stable summary substring `All tests PASSED!`.
- Do not change Pascal source unless a contract exposes an instability.

## Files
- Add: `tests/scripts/test_certificate_p2_core_runtime_contract.sh`
- Add: `docs/plans/2026-03-07-certificate-p2-core-runtime-contract.md`

## Steps
1. RED/Probe
- Validate each file compiles and runs locally.
- Confirm each prints `All tests PASSED!`.
2. GREEN
- Add grouped runtime contract.
- Run `bash -n` then `bash` on the contract.
3. Regression
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- New grouped contract passes without Pascal source changes.
- Existing gates remain green.
