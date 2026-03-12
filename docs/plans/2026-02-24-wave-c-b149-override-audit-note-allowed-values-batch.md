# 2026-02-24 Wave C B149 override audit note 合法值四元组合同

## Goal

- 为 `--override-b147-projected-audit-note` 增加合法值四元组参数化合同。
- 验证每个合法值均可执行成功，且 `sync_state` 语义稳定。

## Architecture / Scope

- Contract only:
  - `tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_audit_note_allowed_values_batch.sh`
- No production behavior change expected.

## Files

- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_audit_note_allowed_values_batch.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. Add contract test:
   - Batch values:
     - `B148_ALERT_WARN_REVIEW_REQUIRED`
     - `B148_ALERT_CLEAR`
     - `B148_ALERT_MISSING`
     - `B148_ALERT_UNKNOWN`
   - For each value:
     - command exits 0
     - report generated
     - summary `b147_projected_b149_audit_alert_note` equals override value
     - `b149_audit_alert_note_sync_state` follows equality with `b149_audit_alert_note`
2. Regression:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_audit_note_allowed_values_batch.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_audit_note_value_validation.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_mismatch_injection.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_consistency.sh`
   - `bash -n tests/scripts/test_wave_c_ci_reenable_submission_bundle_override_audit_note_allowed_values_batch.sh`
3. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round23.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round23.md`
4. Update `task_plan.md` / `findings.md` / `progress.md`.

## Expected Outputs

- 合法值四元组全部通过，语义断言稳定。
- docs strict round23 继续零噪声、零重复。
