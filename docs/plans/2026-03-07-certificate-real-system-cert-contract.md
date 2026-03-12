# 2026-03-07 Certificate Real System-Cert Contract

> Runtime validation from this focused plan was consolidated on 2026-03-07 into `tests/scripts/test_certificate_utilities_runtime_contract.sh`; keep this file as the historical record of the original system-certificate test shape.

## Goal
Stabilize `tests/certificate/test_certificate_real.pas` against environment-dependent system-store ordering while preserving meaningful coverage.

## Architecture
- RED: current runtime assumes the first system certificate always exposes a non-empty serial number
- GREEN: scan for the first readable certificate metadata entry and treat missing serials as graceful, not fatal
- Cover the runtime in the consolidated non-P2 certificate runtime contract while keeping the existing pass summary

## Files
- Modify: `tests/certificate/test_certificate_real.pas`
- Cover via: `tests/scripts/test_certificate_utilities_runtime_contract.sh`

## Steps
1. Run the focused RED command
2. Make the smallest test-side semantic adjustment
3. Re-run focused runtime
4. Regress with `bash scripts/run_minimal_ci_gate.sh --fast-local` and `python3 scripts/compile_all_modules.py`
