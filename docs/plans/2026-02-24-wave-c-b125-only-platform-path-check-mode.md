# 2026-02-24 Wave C B125 Only Platform Path-Check Mode

## Goal

- Add a fast mode for `run_wave_c_local_first_guard_bundle.sh` to run only platform path-check dry-run batch.
- Keep existing default behavior unchanged.

## Architecture / Scope

- Script:
  - `scripts/run_wave_c_local_first_guard_bundle.sh`
  - Add option:
    - `--only-platform-path-check-dryrun`
  - Behavior:
    - default: run B123 + B124 + optional B125A
    - only mode: skip B123/B124 and run only B125A (unless B125A is also explicitly skipped)
  - Report:
    - B123/B124 rows show `SKIP | SKIPPED | <none> | <none>` when only mode enabled.
- Contract:
  - `tests/scripts/test_wave_c_local_first_guard_bundle_only_platform_path_check_mode.sh`
  - Validate:
    - only mode generates platform log only.
    - B123/B124 reports/logs are not generated.
    - step matrix preserves clear skip semantics.

## Files

- `scripts/run_wave_c_local_first_guard_bundle.sh`
- `tests/scripts/test_wave_c_local_first_guard_bundle_only_platform_path_check_mode.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_c_local_first_guard_bundle_only_platform_path_check_mode.sh`
   - Expected: fail (`Unknown option: --only-platform-path-check-dryrun`).
2. GREEN:
   - Implement only mode option and step-matrix skip semantics.
3. Regression:
   - `bash tests/scripts/test_wave_c_local_first_guard_bundle_only_platform_path_check_mode.sh`
   - `bash tests/scripts/test_wave_c_local_first_guard_bundle_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_wave_c_local_guard_oncall_platform_path_checks_passthrough.sh`
   - `bash tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_platform_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_b138_platform_skip_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_skip_local_guard_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_docs_governance_integration.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash -n scripts/run_wave_c_local_first_guard_bundle.sh tests/scripts/test_wave_c_local_first_guard_bundle_only_platform_path_check_mode.sh`

## Expected Outputs

- B125 supports lightweight platform-only validation for local guarding.
- Existing wave-c chain behavior remains backward compatible.
