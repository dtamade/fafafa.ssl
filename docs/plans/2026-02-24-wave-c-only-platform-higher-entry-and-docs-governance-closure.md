# 2026-02-24 Wave C Only-Platform 上接高层入口 + Docs 治理收口

## Goal

- 将 `--only-platform-path-check-dryrun` 继续上接到更高层入口：
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`（B149）
  - `scripts/run_minimal_ci_gate.sh`（minimal gate 可选快速链路）
- 持续执行 `docs/**`（排除 `docs/archive/**`）噪声治理与索引一致性 strict 收口。

## Architecture / Scope

- Scripts:
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
  - `scripts/run_minimal_ci_gate.sh`
  - `tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`
- New option propagation:
  - B149: `--only-platform-path-check-dryrun` -> B144C local guard batch
  - minimal gate: `--only-platform-path-check-dryrun`（仅执行四平台路径检查 dry-run batch）
- Observability:
  - B149 报告记录 `b144c_local_guard_bundle_mode`（`FULL` / `PLATFORM_ONLY`）

## Contracts

- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_platform_option.sh`
- `tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh`

## Files

- `docs/plans/2026-02-24-wave-c-only-platform-higher-entry-and-docs-governance-closure.md`
- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `scripts/run_minimal_ci_gate.sh`
- `tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`
- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_platform_option.sh`
- `tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_platform_option.sh`
   - `bash tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh`
2. GREEN:
   - 在 B149 增加 only-platform 参数解析、透传和报告可观测字段。
   - 在 local guard batch 合同中支持 only-platform 可选矩阵断言。
   - 在 minimal gate 增加 only-platform 快速模式开关。
3. Regression:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_platform_option.sh`
   - `bash tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_skip_local_guard_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_docs_governance_integration.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_local_guard_batch_integration.sh`
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_b138_only_platform_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_b138_platform_skip_passthrough.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_minimal_ci_gate_docs_governance_batch_option.sh`
   - `bash tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh`
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round3.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round3.md`
   - `bash -n scripts/run_wave_c_ci_reenable_submission_bundle.sh scripts/run_minimal_ci_gate.sh tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_platform_option.sh tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh`

## Expected Outputs

- B149 支持 `--only-platform-path-check-dryrun` 并可观测到 local guard bundle mode。
- minimal gate 支持 only-platform 快速模式且默认行为不变。
- docs active noise + docs index dedup 在 strict 下继续为零问题。
