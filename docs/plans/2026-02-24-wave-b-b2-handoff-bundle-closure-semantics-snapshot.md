# 2026-02-24 Wave B B2 Handoff Bundle 增补 Closure Semantics 摘录

## Goal

- 在 `scripts/prepare_wave_b_b2_handoff_bundle.sh` 的输出中新增 `Status Semantics` 摘录，减少审计时在 handoff 与 closure 报告之间来回跳转。
- 保持既有 handoff 状态判定与 strict 语义不变。

## Architecture / Scope

- Script:
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - 新增从 closure 报告提取 `## Status Semantics` 章节正文的 helper
  - handoff 报告新增 `## Closure Semantics Snapshot`
- Contract:
  - `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_semantics_snapshot.sh`
  - 断言 handoff 报告包含 semantics 快照标题、来源路径与关键语义行

## Files

- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_semantics_snapshot.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)

1. RED: 新增 semantics snapshot 合同并运行，确认当前实现失败。
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_semantics_snapshot.sh`
2. GREEN: 在 handoff 生成脚本中实现 semantics 摘录写入。
3. Regression:
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_semantics_snapshot.sh`
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_blocker_artifact_visibility.sh`
   - `bash tests/scripts/test_wave_b_b2_closure_readiness_dryrun_skipped_semantics_contract.sh`
   - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_blocker_linkage_contract.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_android_extension.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
   - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_semantics_snapshot.sh`
4. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round10.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round10.md`
5. 回写 `task_plan.md` / `findings.md` / `progress.md`。

## Expected Outputs

- handoff 报告出现 `## Closure Semantics Snapshot`，并可直接看到 closure 语义条目。
- Wave B 相关合同保持全绿；strict docs 治理保持 `total_hits=0` 与重复项为 0。
