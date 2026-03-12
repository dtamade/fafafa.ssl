# 2026-02-24 Wave C B148 审计提示同步预览上浮

## Goal

- 将 `b149_audit_alert_note_sync_state` 的可读性前移到 B148 审批摘要。
- 在 B148 内直接展示：B147 投影值、B148 预览值、同步状态。
- 不改变门禁判定逻辑，仅增强审批入口审计口径。

## Architecture / Scope

- Script:
  - `scripts/generate_wave_c_ci_reenable_approval_brief.sh`
    - 解析 B147 `projected_b149_audit_alert_note`
    - 计算 `b149_audit_alert_note_preview`
    - 输出 `b149_audit_alert_note_sync_state`（`MATCH/MISMATCH/MISSING`）
- Contract:
  - `tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_audit_note_sync_preview.sh`

## Files

- `scripts/generate_wave_c_ci_reenable_approval_brief.sh`
- `tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_audit_note_sync_preview.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)

1. RED:
   - `bash tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_audit_note_sync_preview.sh`
2. GREEN:
   - 在 B148 增加 `## Audit Note Sync Preview` 区块与三个字段。
3. Regression:
   - `bash tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_audit_note_sync_preview.sh`
   - `bash tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_consistency_alert_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_audit_note_sync_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_audit_alert_headline.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b149_audit_alert_note_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b148_alert_state_summary.sh`
   - `bash -n scripts/generate_wave_c_ci_reenable_approval_brief.sh tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_audit_note_sync_preview.sh`
4. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round19.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round19.md`
5. 回写 `task_plan.md` / `findings.md` / `progress.md`。

## Expected Outputs

- B148 报告具备审计提示同步预览并可区分 `MATCH/MISMATCH/MISSING`。
- B148/B149 关键合同继续全绿；docs strict round19 全绿。
