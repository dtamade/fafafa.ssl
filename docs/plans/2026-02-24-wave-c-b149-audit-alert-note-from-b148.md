# 2026-02-24 Wave C B149 基于 B148 alert_state 的审计提示收口

## Goal

- 在 `scripts/run_wave_c_ci_reenable_submission_bundle.sh` 的 `## Summary` 中新增统一审计提示字段，基于 `b148_alert_state` 输出可读决策口径。
- 保持现有 `overall` 判定逻辑不变，仅增强审计提示可读性。

## Architecture / Scope

- Script:
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
  - 新增 `b149_audit_alert_note` 计算（由 `b148_alert_state` 映射）
  - 在 `## Summary` 增加 `- b149_audit_alert_note: ...`
- Contract:
  - `tests/scripts/test_wave_c_ci_reenable_submission_bundle_b149_audit_alert_note_summary.sh`
  - 从生成的 B148 报告读取 `alert_state`，断言 B149 Summary 的映射字段一致

## Files

- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_b149_audit_alert_note_summary.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)

1. RED:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b149_audit_alert_note_summary.sh`
2. GREEN:
   - 实现 `b149_audit_alert_note` 映射并写入 B149 Summary。
3. Regression:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b149_audit_alert_note_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b148_alert_state_summary.sh`
   - `bash tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_consistency_alert_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_local_guard_batch_integration.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_docs_governance_integration.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_skip_local_guard_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_platform_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_skip_semantics.sh`
   - `bash -n scripts/run_wave_c_ci_reenable_submission_bundle.sh tests/scripts/test_wave_c_ci_reenable_submission_bundle_b149_audit_alert_note_summary.sh`
4. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round16.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round16.md`
5. 回写 `task_plan.md` / `findings.md` / `progress.md`。

## Expected Outputs

- B149 `## Summary` 输出 `b149_audit_alert_note` 且与 B148 `alert_state` 映射一致。
- B148/B149 关键合同保持全绿；docs strict round16 继续零噪声零重复。
