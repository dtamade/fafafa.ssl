> Superseded on 2026-03-07 by `2026-03-07-ocsp-simple-current-loader-contract.md`.

# 2026-03-07 OCSP Simple Manual-Only Contract

## Goal
Keep `tests/certificate/test_ocsp_simple.pas` CI-safe by classifying it as compile-only/manual-only instead of forcing a new non-interactive runtime path.

## Architecture
- `test_ocsp_simple.pas` remains an interactive diagnostic entrypoint.
- Local CI should:
  - compile it successfully
  - verify the source explicitly declares manual-only status
  - verify it is not pulled into runtime contract batches
- OCSP runtime smoke coverage stays in `tests/certificate/test_ocsp_validation.pas`.

## Files
- Add: `tests/scripts/test_ocsp_simple_manual_only_contract.sh`
- Modify: `tests/certificate/test_ocsp_simple.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Steps
1. RED
- `bash tests/scripts/test_ocsp_simple_manual_only_contract.sh`
- Expected: fail because `test_ocsp_simple.pas` does not yet carry an explicit manual-only classification marker.

2. GREEN
- Add an explicit source comment marking the program manual-only / compile-only in CI.
- Re-run:
  - `bash tests/scripts/test_ocsp_simple_manual_only_contract.sh`
- Expected: pass, while keeping runtime OCSP coverage in `test_ocsp_validation.pas`.

3. Regression
- `bash tests/scripts/test_certificate_smoke_runtime_contract.sh`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- `test_ocsp_simple_manual_only_contract.sh` passes.
- `test_ocsp_simple.pas` is explicitly marked manual-only.
- `test_ocsp_simple.pas` compiles but is not run by runtime contract batches.
