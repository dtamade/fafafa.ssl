# 2026-02-24 WinSSL 阻塞批次入口 + B149 only/skip 语义收口

## Goal

- 为 `P1-33~P1-36` 提供可在 Windows/Win64 RTL 实机直接执行的阻塞批次入口与证据报告。
- 收口 B149 `--only-platform-path-check-dryrun` 与 `--skip-local-guard-batch` 组合语义，提升审计可读性。

## Architecture / Scope

- Scripts:
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
  - `scripts/run_windows_winssl_blocker_batch_draft.sh`
- Tests:
  - `tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_skip_semantics.sh`
  - `tests/scripts/test_windows_winssl_blocker_batch_draft_dryrun_contract.sh`
  - `tests/scripts/test_windows_winssl_blocker_batch_draft_failure_contract.sh`

## Contracts

- B149 only+skip 组合语义：
  - skip local guard 优先；
  - only 标记被忽略时应有显式字段说明。
- Windows 阻塞批次入口：
  - dry-run 在 Linux 上可执行并生成报告；
  - 非 dry-run 在非 Windows 环境必须失败并给出明确报错。

## Files

- `docs/plans/2026-02-24-winssl-blocker-batch-entry-and-b149-only-skip-semantics.md`
- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `scripts/run_windows_winssl_blocker_batch_draft.sh`
- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_skip_semantics.sh`
- `tests/scripts/test_windows_winssl_blocker_batch_draft_dryrun_contract.sh`
- `tests/scripts/test_windows_winssl_blocker_batch_draft_failure_contract.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_skip_semantics.sh`
2. GREEN:
   - 更新 B149 报告组合语义字段。
   - 新增 Windows 阻塞批次入口脚本 + dry-run/非 dry-run 合同。
3. Regression:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_skip_semantics.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_platform_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_skip_local_guard_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_docs_governance_integration.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_local_guard_batch_integration.sh`
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_minimal_ci_gate_docs_governance_batch_option.sh`
   - `bash tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh`
   - `bash tests/scripts/test_windows_winssl_blocker_batch_draft_dryrun_contract.sh`
   - `bash tests/scripts/test_windows_winssl_blocker_batch_draft_failure_contract.sh`
   - `bash tests/scripts/test_windows_winssl_path_check_draft_dryrun_contract.sh`
   - `bash tests/scripts/test_windows_winssl_path_check_draft_failure_contract.sh`
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round4.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round4.md`
   - `bash -n scripts/run_wave_c_ci_reenable_submission_bundle.sh scripts/run_windows_winssl_blocker_batch_draft.sh tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_skip_semantics.sh tests/scripts/test_windows_winssl_blocker_batch_draft_dryrun_contract.sh tests/scripts/test_windows_winssl_blocker_batch_draft_failure_contract.sh`

## Expected Outputs

- B149 报告能明确表达 only+skip 组合语义（skip 优先、only ignored 可见）。
- WinSSL 阻塞批次在 Windows 实机可直接执行并生成结构化报告；在非 Windows 有可验证失败合同。
