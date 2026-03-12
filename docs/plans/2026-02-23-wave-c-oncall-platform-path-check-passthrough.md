# 2026-02-23 Wave C Oncall Platform Path-Check Passthrough

## Goal

- Ensure Wave C oncall entrypoint (`run_wave_c_local_guard_oncall_check.sh`) is consistent with B125 platform path-check integration.
- Add a contract test that locks default path-check execution and skip passthrough behavior.

## Architecture / Scope

- Script:
  - `scripts/run_wave_c_local_guard_oncall_check.sh`
  - Add option:
    - `--skip-platform-path-checks-dryrun`
  - Forward this option to:
    - `scripts/run_wave_c_local_first_guard_bundle.sh`
  - Parse and expose B125 platform path-check state in oncall report checks table.
- Contract:
  - `tests/scripts/test_wave_c_local_guard_oncall_platform_path_checks_passthrough.sh`
  - Validate:
    - default run generates B125 platform path-check log.
    - skip run suppresses B125 platform path-check log and yields `SKIP/SKIPPED`.

## Files

- `scripts/run_wave_c_local_guard_oncall_check.sh`
- `tests/scripts/test_wave_c_local_guard_oncall_platform_path_checks_passthrough.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_c_local_guard_oncall_platform_path_checks_passthrough.sh`
   - Expected: fail (oncall script does not support skip passthrough option).
2. GREEN:
   - Implement skip passthrough + report row in `run_wave_c_local_guard_oncall_check.sh`.
3. Regression:
   - `bash tests/scripts/test_wave_c_local_guard_oncall_platform_path_checks_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_first_guard_bundle_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash -n scripts/run_wave_c_local_guard_oncall_check.sh tests/scripts/test_wave_c_local_guard_oncall_platform_path_checks_passthrough.sh`

## Expected Outputs

- Oncall report includes B125 platform path-check state row.
- Oncall skip option deterministically bypasses B125 platform path-check step while preserving report consistency.
