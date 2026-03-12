# 2026-02-24 Wave C B149 override audit note 值域校验

## Goal

- 为 `--override-b147-projected-audit-note` 增加值域约束。
- 非法值应立即失败（exit non-zero），防止误用污染审计口径。

## Architecture / Scope

- Script:
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
    - 校验 `--override-b147-projected-audit-note` 仅允许：
      - `B148_ALERT_WARN_REVIEW_REQUIRED`
      - `B148_ALERT_CLEAR`
      - `B148_ALERT_MISSING`
      - `B148_ALERT_UNKNOWN`
- Contract:
  - `tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_audit_note_value_validation.sh`

## Files

- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_audit_note_value_validation.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)

1. RED:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_audit_note_value_validation.sh`
2. GREEN:
   - 增加 override 值域校验与错误提示。
3. Regression:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_audit_note_value_validation.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_mismatch_injection.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_consistency.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_audit_note_sync_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_audit_alert_headline.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b149_audit_alert_note_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b148_alert_state_summary.sh`
   - `bash -n scripts/run_wave_c_ci_reenable_submission_bundle.sh tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_audit_note_value_validation.sh`
4. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round22.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round22.md`
5. 回写 `task_plan.md` / `findings.md` / `progress.md`。

## Expected Outputs

- 非法 override 值立即失败且错误信息可读。
- 合法 override（现有 mismatch 合同）不受影响。
- docs strict round22 继续全绿。
