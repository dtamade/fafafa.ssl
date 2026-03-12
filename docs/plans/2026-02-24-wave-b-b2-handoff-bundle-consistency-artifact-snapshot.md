# 2026-02-24 Wave B B2 Handoff Bundle 增补 Consistency Artifact Snapshot

## Goal

- 在 `scripts/prepare_wave_b_b2_handoff_bundle.sh` 的 handoff 输出中加入 consistency `Artifact Matrix` 关键行摘录。
- 关键行覆盖：`linux_summary`、`cross_summary`、`closure_report`、`windows_summary`、`windows_blocker_batch_report`。

## Architecture / Scope

- Script:
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - 扩展 consistency 字段解析能力（`run_id_match`、`note`）
  - handoff 报告新增 `## Consistency Artifact Snapshot`
- Contract:
  - `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_consistency_artifact_snapshot.sh`
  - 校验 snapshot 标题、来源、关键行（含 blocker 行）可见

## Files

- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_consistency_artifact_snapshot.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)

1. RED:
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_consistency_artifact_snapshot.sh`
2. GREEN:
   - 实现 consistency 快照区块与关键行渲染。
3. Regression:
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
   - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh tests/scripts/test_prepare_wave_b_b2_handoff_bundle_consistency_artifact_snapshot.sh`
4. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round12.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round12.md`
5. 回写 `task_plan.md` / `findings.md` / `progress.md`。

## Expected Outputs

- handoff 报告出现 `## Consistency Artifact Snapshot`，可直接查看 blocker 证据行与 run_id 匹配口径。
- Wave B 合同维持全绿；docs strict round12 继续零噪声零重复。
