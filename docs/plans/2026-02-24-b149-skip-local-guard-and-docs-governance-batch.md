# 2026-02-24 B149 Skip Local Guard Batch + Docs Governance Batch

## Goal

- Improve B149 operability with a fast-path option while keeping full verification as default.
- Continue docs consistency closure by creating one strict batch gate for active docs noise + index dedup.

## Architecture / Scope

- Script:
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
  - Add options:
    - `--skip-local-guard-batch` (skip B144C local guard skip-matrix batch)
    - `--skip-docs-governance` (skip docs strict batch)
  - Keep defaults:
    - run local guard batch by default
    - run docs governance batch by default
- New batch contract:
  - `tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh`
  - Runs strict checks:
    - `scripts/scan_active_docs_noise_draft.sh --strict`
    - `scripts/check_docs_index_dedup_draft.sh --scope all --strict`
- Contracts:
  - `tests/scripts/test_wave_c_ci_reenable_submission_bundle_skip_local_guard_option.sh`
    - Verify default/skip behavior for local guard batch.
  - `tests/scripts/test_wave_c_ci_reenable_submission_bundle_docs_governance_integration.sh`
    - Verify B149 executes docs governance batch by default and skip option works.

## Files

- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh`
- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_skip_local_guard_option.sh`
- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_docs_governance_integration.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_skip_local_guard_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_docs_governance_integration.sh`
2. GREEN:
   - Implement options and step matrix rows in B149.
   - Add docs governance strict batch script.
3. Regression:
   - `bash tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_skip_local_guard_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_docs_governance_integration.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_local_guard_batch_integration.sh`
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_b138_platform_skip_passthrough.sh`
   - `bash tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_platform_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_guard_oncall_platform_path_checks_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_first_guard_bundle_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash -n scripts/run_wave_c_ci_reenable_submission_bundle.sh tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh tests/scripts/test_wave_c_ci_reenable_submission_bundle_skip_local_guard_option.sh tests/scripts/test_wave_c_ci_reenable_submission_bundle_docs_governance_integration.sh`

## Expected Outputs

- B149 supports both full verification (default) and fast-path skip mode for local guard/docs governance.
- Docs governance strict checks are reusable through a single batch contract and auditable in B149 report/logs.
