# 2026-02-23 Wave C Local Guard Bundle Platform Path-Check Integration

## Goal

- Integrate the four-platform dry-run path-check chain into the higher-level local guard entrypoint `run_wave_c_local_first_guard_bundle.sh`.
- Add an integration contract test that locks default-on behavior and skip-switch behavior.

## Architecture / Scope

- Entry script:
  - `scripts/run_wave_c_local_first_guard_bundle.sh`
  - Add one new step after B123/B124:
    - execute `scripts/run_minimal_ci_gate.sh --skip-compile --skip-modules --skip-phase2-dryrun`
  - Add switch:
    - `--skip-platform-path-checks-dryrun`
- Contract:
  - `tests/scripts/test_wave_c_local_first_guard_bundle_platform_path_checks_integration.sh`
  - Validate:
    - default run generates platform step log and contains batch-pass marker.
    - skip run does not generate platform log and records `SKIP/SKIPPED` row in bundle report.

## Files

- `scripts/run_wave_c_local_first_guard_bundle.sh`
- `tests/scripts/test_wave_c_local_first_guard_bundle_platform_path_checks_integration.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_c_local_first_guard_bundle_platform_path_checks_integration.sh`
   - Expected: fail (missing platform-step integration/log).
2. GREEN:
   - Implement platform-step integration + skip option in `run_wave_c_local_first_guard_bundle.sh`.
3. Regression:
   - `bash tests/scripts/test_wave_c_local_first_guard_bundle_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash -n scripts/run_wave_c_local_first_guard_bundle.sh tests/scripts/test_wave_c_local_first_guard_bundle_platform_path_checks_integration.sh`

## Expected Outputs

- Wave C B125 bundle includes platform path-check step by default and reflects its exit/state in step matrix.
- Skip mode can bypass this step deterministically without side effects.
