# 2026-02-24 Wave C B149 透传 B148 Audit Sync State 并收口三段一致性

## Goal

- 将 B148 的 `b149_audit_alert_note_sync_state` 透传到 B149 `## Summary`。
- 在 B149 增加三段链路一致性结果：`B147 -> B148 -> B149`。
- 不改变 gate 判定逻辑，仅增强审计可观测性。

## Architecture / Scope

- Script:
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
    - 新增读取 B148 字段：`b149_audit_alert_note_preview` / `b149_audit_alert_note_sync_state`
    - 新增摘要字段：
      - `b148_b149_audit_alert_note_sync_state`
      - `b147_b148_b149_audit_note_consistency`
- Contract:
  - `tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_consistency.sh`

## Files

- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_consistency.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)

1. RED:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_consistency.sh`
2. GREEN:
   - 在 B149 透传 B148 `b149_audit_alert_note_sync_state`。
   - 计算并输出三段一致性字段。
3. Regression:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_consistency.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_audit_note_sync_summary.sh`
   - `bash tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_audit_note_sync_preview.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_audit_alert_headline.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b149_audit_alert_note_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b148_alert_state_summary.sh`
   - `bash -n scripts/run_wave_c_ci_reenable_submission_bundle.sh tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_consistency.sh`
4. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round20.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round20.md`
5. 回写 `task_plan.md` / `findings.md` / `progress.md`。

## Expected Outputs

- B149 摘要展示 B148 同步态透传值。
- B149 摘要展示 `b147_b148_b149_audit_note_consistency` 且稳定可机读。
- 关键合同全绿；docs strict round20 全绿。
