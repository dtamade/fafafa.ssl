# 2026-02-24 Wave C Unified Reports Dir + Skip Matrix Batch Contract

## Goal

- Unify `--reports-dir` behavior across Wave C B125/B129/B138/B144 chain.
- Add one batch contract that validates default/skip matrix under a custom reports directory.

## Architecture / Scope

- Scripts:
  - `scripts/run_wave_c_local_guard_oncall_check.sh` (B129)
  - `scripts/run_wave_c_pre_ci_reenable_full_gate.sh` (B138)
  - `scripts/run_wave_c_local_guard_ops_pack.sh` (B144)
- Existing support reused:
  - `scripts/run_wave_c_local_first_guard_bundle.sh` (B125 already has `--reports-dir`)
- Contract:
  - `tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`
  - Validate:
    - custom `--reports-dir` keeps B144/B138/B129/B125 artifacts in the same directory.
    - default mode generates B125 platform path-check log.
    - skip mode suppresses B125 platform path-check log and preserves `SKIP/SKIPPED` semantics.

## Files

- `scripts/run_wave_c_local_guard_oncall_check.sh`
- `scripts/run_wave_c_pre_ci_reenable_full_gate.sh`
- `scripts/run_wave_c_local_guard_ops_pack.sh`
- `tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`
   - Expected: fail (`Unknown option: --reports-dir` on B144).
2. GREEN:
   - Add `--reports-dir` to B129/B138/B144.
   - Pass `--reports-dir` through B144 -> B138 -> B129 -> B125.
   - Keep defaults backward compatible (`test-reports`).
3. Regression:
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_b138_platform_skip_passthrough.sh`
   - `bash tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_platform_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_guard_oncall_platform_path_checks_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_first_guard_bundle_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash -n scripts/run_wave_c_local_guard_oncall_check.sh scripts/run_wave_c_pre_ci_reenable_full_gate.sh scripts/run_wave_c_local_guard_ops_pack.sh tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`

## Expected Outputs

- One top-level flag (`--reports-dir`) can isolate Wave C full chain artifacts away from `test-reports`.
- Full-chain default/skip matrix remains observable and deterministic.
