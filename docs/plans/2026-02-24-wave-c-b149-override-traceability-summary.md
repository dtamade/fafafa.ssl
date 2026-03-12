# 2026-02-24 Wave C B149 override 使用痕迹摘要化

## Goal

- 在 B149 `## Summary` 增加 override 使用痕迹字段。
- 明确当前 `b147_projected_b149_audit_alert_note` 的来源与覆盖值，便于审计追踪。

## Architecture / Scope

- Script:
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
    - 新增摘要字段：
      - `b147_projected_audit_note_source`（`B147_REPORT` / `OVERRIDE`）
      - `b147_projected_audit_note_override_value`（`NONE` 或 override 值）
- Contract:
  - `tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_traceability_summary.sh`
    - case default: source=`B147_REPORT`, override_value=`NONE`
    - case override: source=`OVERRIDE`, override_value=传入值

## Files

- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_traceability_summary.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)

1. RED:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_traceability_summary.sh`
2. GREEN:
   - 增加两个摘要字段并接入 override 分支。
3. Regression:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_traceability_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_audit_note_allowed_values_batch.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_audit_note_value_validation.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_mismatch_injection.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_consistency.sh`
   - `bash -n scripts/run_wave_c_ci_reenable_submission_bundle.sh tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_traceability_summary.sh`
4. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round24.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round24.md`
5. Update `task_plan.md` / `findings.md` / `progress.md`.

## Expected Outputs

- B149 摘要可明确识别 override 是否启用。
- 既有 override/一致性合同继续全绿。
