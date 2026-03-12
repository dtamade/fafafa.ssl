# 2026-02-24 Wave C B147/B149 审计提示跨文档同步收口

## Goal

- 在 B147 报告中产出可机读的下游审计提示投影（B148 alert + B149 audit note）。
- 在 B149 报告中显式展示“B147 投影值 vs B149 实际值”的一致性结果，形成可审计闭环。
- 不改变门禁判定逻辑，仅增强审计可读性与跨文档口径一致性。

## Architecture / Scope

- Script:
  - `scripts/check_wave_c_ci_reenable_submission_pack.sh`
    - 新增 `projected_b148_alert_state` / `projected_b149_audit_alert_note` 字段。
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
    - 读取 B147 投影值，输出 `b147_projected_b149_audit_alert_note` 与 `b149_audit_alert_note_sync_state`。
- Contract:
  - `tests/scripts/test_wave_c_ci_reenable_submission_pack_check_audit_note_projection.sh`
  - `tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_audit_note_sync_summary.sh`

## Files

- `scripts/check_wave_c_ci_reenable_submission_pack.sh`
- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `tests/scripts/test_wave_c_ci_reenable_submission_pack_check_audit_note_projection.sh`
- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_audit_note_sync_summary.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)

1. RED:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_pack_check_audit_note_projection.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_audit_note_sync_summary.sh`
2. GREEN:
   - 在 B147 生成投影字段。
   - 在 B149 读取并比对 B147 投影字段，输出同步状态。
3. Regression:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_pack_check_audit_note_projection.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_audit_note_sync_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_audit_alert_headline.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b149_audit_alert_note_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b148_alert_state_summary.sh`
   - `bash tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_consistency_alert_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_skip_local_guard_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_platform_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_skip_semantics.sh`
   - `bash -n scripts/check_wave_c_ci_reenable_submission_pack.sh scripts/run_wave_c_ci_reenable_submission_bundle.sh tests/scripts/test_wave_c_ci_reenable_submission_pack_check_audit_note_projection.sh tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_audit_note_sync_summary.sh`
4. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round18.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round18.md`
5. 回写 `task_plan.md` / `findings.md` / `progress.md`。

## Expected Outputs

- B147 报告包含投影字段，且 PASS/FAIL 场景映射稳定。
- B149 报告包含投影值与一致性状态（预期 `MATCH`）。
- B148/B149 既有合同继续全绿，docs strict round18 继续零噪声零重复。
