# OCSP Focused Contract Drift Observability

## Goal
Make the second focused zero-noise contract resilient to OCSP test-count drift while keeping failure detection strict.

## Architecture
- Keep compile noise gate unchanged:
  - fail on any warning/note in compile output
- Replace brittle runtime marker check with stable summary parsing:
  - parse `Passed:` and `Failed:` counts from runtime summary
  - require `Failed = 0`
  - require `Passed >= 1`
  - emit `[INFO]` drift snapshot (`passed=<N> failed=<M>`) for observability

## Scope
- Modify: `tests/scripts/test_focused_compile_zero_noise_ocsp_regression_contract.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - tighten contract expectations to require summary parsing fields
   - run:
     - `bash tests/scripts/test_focused_compile_zero_noise_ocsp_regression_contract.sh`
   - expected: fail if parser assumptions are wrong
2. GREEN:
   - implement runtime summary extraction and validations
3. Regression:
   - `bash -n tests/scripts/test_focused_compile_zero_noise_ocsp_regression_contract.sh`
   - `bash tests/scripts/test_focused_compile_zero_noise_ocsp_regression_contract.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- OCSP focused contract passes without relying on a single fragile case-level PASS marker.
- Runtime drift remains observable via emitted summary snapshot.
- Governance batch and compile gate remain green (`179/179`, `0 failed`).
