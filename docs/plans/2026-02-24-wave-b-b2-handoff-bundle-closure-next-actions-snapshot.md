# 2026-02-24 Wave B B2 Handoff Bundle 增补 Closure Next Actions 摘录

## Goal

- 在 `scripts/prepare_wave_b_b2_handoff_bundle.sh` 产出的 handoff 报告中补充 closure `## Next Actions` 摘录。
- 形成 handoff 内“状态语义 + 执行动作”一页视图，降低审计/交接阅读成本。

## Architecture / Scope

- Script:
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - 复用已有 markdown section 提取 helper，新增 `## Closure Next Actions Snapshot`
- Contract:
  - `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_next_actions_snapshot.sh`
  - 断言 handoff 中包含 next actions 标题、来源和关键动作行

## Files

- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_next_actions_snapshot.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)

1. RED: 新增 next-actions snapshot 合同并运行，确认失败。
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_next_actions_snapshot.sh`
2. GREEN: handoff 脚本新增 closure next actions 摘录区块。
3. Regression:
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_next_actions_snapshot.sh`
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_semantics_snapshot.sh`
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_blocker_artifact_visibility.sh`
   - `bash tests/scripts/test_wave_b_b2_closure_readiness_dryrun_skipped_semantics_contract.sh`
   - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_blocker_linkage_contract.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_android_extension.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
   - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_next_actions_snapshot.sh`
4. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round11.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round11.md`
5. 回写 `task_plan.md` / `findings.md` / `progress.md`。

## Expected Outputs

- handoff 报告新增 `## Closure Next Actions Snapshot` 并展示 closure 原始行动项。
- Wave B 关键合同持续全绿；docs strict round11 保持零噪声零重复。
