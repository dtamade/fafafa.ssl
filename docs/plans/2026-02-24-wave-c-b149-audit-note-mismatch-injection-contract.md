# 2026-02-24 Wave C B149 三段一致性 MISMATCH 注入合同

## Goal

- 为 B149 三段一致性补齐可重复触发的 `MISMATCH` 合同。
- 在不改变默认门禁行为前提下，提供测试侧可控注入入口。

## Architecture / Scope

- Script:
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
    - 新增可选参数：`--override-b147-projected-audit-note VALUE`
    - 仅覆盖 B149 聚合时读取到的 `b147_projected_b149_audit_alert_note`
- Contract:
  - `tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_mismatch_injection.sh`
    - 注入覆盖值，断言：
      - `b149_audit_alert_note_sync_state: MISMATCH`
      - `b147_b148_b149_audit_note_consistency: MISMATCH`

## Files

- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_mismatch_injection.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)

1. RED:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_mismatch_injection.sh`
2. GREEN:
   - B149 增加 `--override-b147-projected-audit-note` 参数并接入一致性计算。
3. Regression:
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_mismatch_injection.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_consistency.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_audit_note_sync_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_audit_alert_headline.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b149_audit_alert_note_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b148_alert_state_summary.sh`
   - `bash -n scripts/run_wave_c_ci_reenable_submission_bundle.sh tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_mismatch_injection.sh`
4. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round21.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round21.md`
5. 回写 `task_plan.md` / `findings.md` / `progress.md`。

## Expected Outputs

- B149 三段一致性具备 `MATCH` 与 `MISMATCH` 双合同覆盖。
- 默认调用行为不变；docs strict round21 继续全绿。
