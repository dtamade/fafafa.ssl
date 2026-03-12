# Minimal Gate Warning-Noise Default-On Decision

## Goal
Decide and implement warning-noise governance as a default step in `run_minimal_ci_gate.sh`, while retaining an explicit skip switch for speed-sensitive scenarios.

## Decision
- **Default ON** for warning-noise governance batch:
  - improves baseline stability
  - reduces manual omission risk
- Provide explicit opt-out:
  - `--skip-warning-noise-governance-batch`
- Keep quick isolation mode stable:
  - `--only-platform-path-check-dryrun` must still suppress warning-noise governance batch

## Scope
- Modify: `scripts/run_minimal_ci_gate.sh`
- Modify: `tests/scripts/test_minimal_ci_gate_warning_noise_governance_batch_option.sh`
- Modify: `tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - update warning-noise option contract to expect default invocation and skip override
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_warning_noise_governance_batch_option.sh`
   - expected: fail (current default is off and skip flag missing)
2. GREEN:
   - set warning-noise batch default to `true`
   - add parser branch for `--skip-warning-noise-governance-batch`
   - update usage text
3. Regression:
   - `bash -n scripts/run_minimal_ci_gate.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_warning_noise_governance_batch_option.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh`
   - `bash tests/scripts/test_minimal_ci_gate_warning_noise_governance_batch_option.sh`
   - `bash tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- default dry-run includes warning-noise governance batch command.
- `--skip-warning-noise-governance-batch` removes that command.
- only-platform mode remains isolated from warning-noise governance execution.
- module compile gate remains green (`179/179`, `0 failed`).
