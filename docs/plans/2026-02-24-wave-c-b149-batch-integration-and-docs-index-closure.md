# 2026-02-24 Wave C B149 Batch Integration + Docs Index Closure

## Goal

- Connect `tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh` into a higher-level local total validation entry.
- Continue `docs/**` (excluding `docs/archive/**`) noise and index consistency closure.

## Architecture / Scope

- Higher-level local entry selected:
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh` (B149)
  - Reason: it is above B144 in Wave C local guard delivery chain and does not recursively call minimal gate.
- Add one preflight validation step in B149:
  - `tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`
- Contract:
  - `tests/scripts/test_wave_c_ci_reenable_submission_bundle_local_guard_batch_integration.sh`
  - Validate B149 report/log includes the new batch step execution evidence.
- Docs closure:
  - Remove duplicate index entry in `docs/DOCUMENTATION_INDEX.md`.
  - Re-run active docs noise + docs index dedup (strict) scans.

## Files

- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_local_guard_batch_integration.sh`
- `docs/DOCUMENTATION_INDEX.md`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_local_guard_batch_integration.sh`
   - Expected: fail because B149 does not run the local-guard skip-matrix batch yet.
2. GREEN:
   - Update B149 to run `test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`.
   - Add step row and strict gating in B149 report.
3. Regression:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_local_guard_batch_integration.sh`
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_b138_platform_skip_passthrough.sh`
   - `bash tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_platform_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_guard_oncall_platform_path_checks_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_first_guard_bundle_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash -n scripts/run_wave_c_ci_reenable_submission_bundle.sh tests/scripts/test_wave_c_ci_reenable_submission_bundle_local_guard_batch_integration.sh`
4. Docs closure:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round2.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round2.md`

## Expected Outputs

- B149 becomes a higher-level local total validation entry that includes Wave C local-guard reports-dir skip-matrix batch evidence.
- Active docs noise remains zero and docs index dedup reaches strict-pass.
