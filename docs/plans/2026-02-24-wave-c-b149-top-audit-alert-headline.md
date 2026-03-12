# 2026-02-24 Wave C B149 顶部审计告警提示收口

## Goal

- 将 `b149_audit_alert_note` 从 `## Summary` 提升到 B149 报告顶部元信息区域，提升审批入口可见性。
- 保持 `overall` 与门禁判定逻辑不变，仅增强展示层。

## Architecture / Scope

- Script:
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
  - 在报告头部增加 `- audit_alert_note: **...**`
- Contract:
  - `tests/scripts/test_wave_c_ci_reenable_submission_bundle_audit_alert_headline.sh`
  - 基于 B148 `alert_state` 推导期望值，断言 B149 前 20 行包含对应 headline

## Files

- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_audit_alert_headline.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)

1. RED:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_audit_alert_headline.sh`
2. GREEN:
   - 在 B149 顶部元信息增加 `audit_alert_note`。
3. Regression:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_audit_alert_headline.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b149_audit_alert_note_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b148_alert_state_summary.sh`
   - `bash tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_consistency_alert_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_local_guard_batch_integration.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_docs_governance_integration.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_skip_local_guard_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_platform_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_skip_semantics.sh`
   - `bash -n scripts/run_wave_c_ci_reenable_submission_bundle.sh tests/scripts/test_wave_c_ci_reenable_submission_bundle_audit_alert_headline.sh`
4. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round17.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round17.md`
5. 回写 `task_plan.md` / `findings.md` / `progress.md`。

## Expected Outputs

- B149 报告头部包含 `audit_alert_note`，并与 B148 告警态映射一致。
- B148/B149 关键合同保持全绿；docs strict round17 继续零噪声零重复。
