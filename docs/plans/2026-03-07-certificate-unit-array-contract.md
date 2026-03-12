# 2026-03-07 Certificate Unit Array Contract

> Superseded on 2026-03-07 by `tests/scripts/test_certificate_utilities_runtime_contract.sh` and `docs/plans/2026-03-07-certificate-nonp2-runtime-consolidation.md`.

## Goal
Align `tests/certificate/test_certificate_unit.pas` with the current certificate API surface where SAN/KeyUsage accessors return `TSSLStringArray` instead of `TStringList`.

## Architecture
- RED: current compile fails on `.Count`, `.Free`, `.IndexOf` against `TSSLStringArray`
- GREEN: keep the change test-side only
  - add small array helper(s)
  - switch count/contains checks to array semantics
- Cover the corrected entrypoint in the consolidated non-P2 certificate runtime contract

## Files
- Modify: `tests/certificate/test_certificate_unit.pas`
- Cover via: `tests/scripts/test_certificate_utilities_runtime_contract.sh`

## Steps
1. `fpc -Fu./src tests/certificate/test_certificate_unit.pas -otmp/test_certificate_unit_fix`
2. Update the test to use `TSSLStringArray` semantics
3. Re-run compile + runtime
4. Run `bash tests/scripts/test_certificate_utilities_runtime_contract.sh`
5. Regress with `bash scripts/run_minimal_ci_gate.sh --fast-local` and `python3 scripts/compile_all_modules.py`
