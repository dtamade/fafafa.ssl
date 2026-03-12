# 2026-02-24 Wave C Only-Platform Mode Passthrough (B129/B138/B144)

## Goal

- Propagate `--only-platform-path-check-dryrun` from B129/B138/B144 down to B125.
- Keep default behavior unchanged and preserve report observability.

## Architecture / Scope

- Scripts:
  - `scripts/run_wave_c_local_guard_oncall_check.sh` (B129)
  - `scripts/run_wave_c_pre_ci_reenable_full_gate.sh` (B138)
  - `scripts/run_wave_c_local_guard_ops_pack.sh` (B144)
- New option:
  - `--only-platform-path-check-dryrun`
- Passthrough chain:
  - B144 -> B138 -> B129 -> B125
- Report observability:
  - B144 options section adds `b138_local_first_bundle_mode` (`FULL`/`PLATFORM_ONLY`).

## Contracts

- `tests/scripts/test_wave_c_local_guard_oncall_only_platform_path_check_passthrough.sh`
- `tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_only_platform_passthrough.sh`
- `tests/scripts/test_wave_c_local_guard_ops_pack_b138_only_platform_passthrough.sh`

Each contract validates that when only-mode is enabled:
- B125 step matrix shows B123/B124 as `SKIP/SKIPPED` and no B123/B124 artifacts are generated.
- B125 platform path-check log is still generated (unless explicitly skipped).

## Files

- `scripts/run_wave_c_local_guard_oncall_check.sh`
- `scripts/run_wave_c_pre_ci_reenable_full_gate.sh`
- `scripts/run_wave_c_local_guard_ops_pack.sh`
- `tests/scripts/test_wave_c_local_guard_oncall_only_platform_path_check_passthrough.sh`
- `tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_only_platform_passthrough.sh`
- `tests/scripts/test_wave_c_local_guard_ops_pack_b138_only_platform_passthrough.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_c_local_guard_oncall_only_platform_path_check_passthrough.sh`
   - `bash tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_only_platform_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_b138_only_platform_passthrough.sh`
2. GREEN:
   - Implement option parsing + passthrough in B129/B138/B144.
   - Add B144 options row for only-mode observability.
3. Regression:
   - `bash tests/scripts/test_wave_c_local_guard_oncall_only_platform_path_check_passthrough.sh`
   - `bash tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_only_platform_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_b138_only_platform_passthrough.sh`
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
   - `bash -n scripts/run_wave_c_local_guard_oncall_check.sh scripts/run_wave_c_pre_ci_reenable_full_gate.sh scripts/run_wave_c_local_guard_ops_pack.sh tests/scripts/test_wave_c_local_guard_oncall_only_platform_path_check_passthrough.sh tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_only_platform_passthrough.sh tests/scripts/test_wave_c_local_guard_ops_pack_b138_only_platform_passthrough.sh`

## Expected Outputs

- High-level wave-c entry scripts can trigger B125 platform-only mode directly.
- Existing skip/default semantics remain backward compatible.
