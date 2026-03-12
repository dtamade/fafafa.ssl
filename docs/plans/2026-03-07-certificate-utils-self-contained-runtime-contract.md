# 2026-03-07 Certificate Utils Self-Contained Runtime Contract

> Superseded on 2026-03-07 by `tests/scripts/test_certificate_utilities_runtime_contract.sh` and `docs/plans/2026-03-07-certificate-nonp2-runtime-consolidation.md`.

## Goal
Add stable ASCII completion markers and a grouped runtime contract for the self-contained certificate utility programs.

## Architecture
- RED: the consolidated non-P2 certificate runtime contract expects stable `[PASS]` markers that do not exist yet
- GREEN: add one stable marker to each successful program end-state and verify through a focused contract

## Files
- Modify: `tests/certificate/test_cert_utils_enterprise.pas`
- Modify: `tests/certificate/test_cert_utils_try.pas`
- Cover via: `tests/scripts/test_certificate_utilities_runtime_contract.sh`

## Steps
1. Add the grouped runtime contract and observe RED
2. Add stable `[PASS]` markers to both programs
3. Re-run the contract
4. Regress with `bash scripts/run_minimal_ci_gate.sh --fast-local` and `python3 scripts/compile_all_modules.py`
