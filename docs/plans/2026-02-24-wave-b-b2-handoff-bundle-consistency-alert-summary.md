# 2026-02-24 Wave B B2 Handoff Bundle 增补 Consistency Alert Summary

## Goal

- 在 `scripts/prepare_wave_b_b2_handoff_bundle.sh` 顶部增加 consistency 告警摘要，突出关键 mismatch/缺失信号。
- 告警摘要直接基于 consistency 报告已有指标与关键 artifact 行，减少审计阅读路径。

## Architecture / Scope

- Script:
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - 解析 consistency 指标：`required_missing`、`runid_mismatch_or_parse_issue`
  - 新增 `## Consistency Alert Summary` 区块
- Contract:
  - `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_consistency_alert_summary.sh`
  - 锁定告警摘要字段与 blocker 缺失场景的关键行

## Files

- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_consistency_alert_summary.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)

1. RED:
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_consistency_alert_summary.sh`
2. GREEN:
   - 新增 consistency alert summary 区块与关键告警行渲染。
3. Regression:
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_consistency_alert_summary.sh`
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_consistency_artifact_snapshot.sh`
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_next_actions_snapshot.sh`
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_semantics_snapshot.sh`
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_blocker_artifact_visibility.sh`
   - `bash tests/scripts/test_wave_b_b2_closure_readiness_dryrun_skipped_semantics_contract.sh`
   - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_blocker_linkage_contract.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_android_extension.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
   - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh tests/scripts/test_prepare_wave_b_b2_handoff_bundle_consistency_alert_summary.sh`
4. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round13.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round13.md`
5. 回写 `task_plan.md` / `findings.md` / `progress.md`。

## Expected Outputs

- handoff 报告出现 `## Consistency Alert Summary`，可直接看到 required/mismatch 指标与关键告警行。
- Wave B 合同持续全绿；docs strict round13 继续零噪声零重复。
