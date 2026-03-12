# 2026-02-23 Wave C B138 Oncall Platform Skip Passthrough

## Goal

- Continue upward integration: make `run_wave_c_pre_ci_reenable_full_gate.sh` support platform path-check skip passthrough to B129/B125 chain.
- Add a contract test that locks default-enabled and skip passthrough behavior.

## Architecture / Scope

- Script:
  - `scripts/run_wave_c_pre_ci_reenable_full_gate.sh`
  - Add option:
    - `--skip-platform-path-checks-dryrun`
  - Forward to oncall step:
    - `scripts/run_wave_c_local_guard_oncall_check.sh ... --skip-platform-path-checks-dryrun`
- Contract:
  - `tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_platform_passthrough.sh`
  - Validate:
    - default run generates B125 platform path-check log.
    - skip run suppresses B125 platform path-check log and preserves `SKIP/SKIPPED` semantics in B125/B129 reports.

## Files

- `scripts/run_wave_c_pre_ci_reenable_full_gate.sh`
- `tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_platform_passthrough.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_platform_passthrough.sh`
   - Expected: fail (`Unknown option: --skip-platform-path-checks-dryrun`).
2. GREEN:
   - Implement skip option and passthrough to oncall step in B138 script.
3. Regression:
   - `bash tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_platform_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_guard_oncall_platform_path_checks_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_first_guard_bundle_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash -n scripts/run_wave_c_pre_ci_reenable_full_gate.sh tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_platform_passthrough.sh`

## Expected Outputs

- B138 can deterministically run with or without platform path-check chain.
- Skip behavior remains consistent across B138 -> B129 -> B125 layers.
