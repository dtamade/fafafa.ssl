# Wave B Examples Explicit Override Warning Plan

**Goal**
- 当 Linux examples 走 `explicit_override` 时，在 `handoff bundle` 和 `evidence consistency` 中给 reviewer 明确 warning。
- 保持 default discovery 行为不变，只增强 reviewer 可见性。

**Architecture**
- 基于现有 `linux_examples_selection` 派生 `linux_examples_warning`。
- 当 selection=`explicit_override` 时，warning 固定为：`explicit override in use; verify owner run_id/path manually`。
- 其他 selection 统一输出 `none`，避免 reviewer 误以为还存在隐式 override。
- 这波不改 strict 判定，也不改 cross-summary 的计算逻辑；只增强 handoff / evidence 的 reviewer 提示。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-examples-explicit-override-warning.md`
- Add: `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_examples_warning_explicit_override_contract.sh`
- Add: `tests/scripts/test_wave_b_b2_evidence_consistency_examples_warning_explicit_override_contract.sh`
- Add: `tests/scripts/test_wave_b_b2_evidence_consistency_examples_warning_default_none_contract.sh`
- Modify: `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- Modify: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 explicit override warning / default none 合同并确认 RED。
2. 让 handoff / evidence 输出 `linux_examples_warning`。
3. 跑 focused 合同和 selection/default/run-id 回归。
4. 回写 working memory 与当前汇总。

**Expected Outputs**
- reviewer 能在 handoff / evidence 里直接看到 explicit override warning。
- default path 会明确标成 `none`，避免 warning 状态再靠脑补。
- 下一波可以继续给 retention policy 加轻量 contract，或单开历史存量清理。
