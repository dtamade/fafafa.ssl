# Wave B Examples Selection Observability Plan

**Goal**
- 让 `cross-summary`、`handoff bundle`、`evidence checker` 明确写出 Linux examples 证据是怎么被选中的。
- 把显式覆盖与默认发现区分开，减少 review 时对 `--linux-examples` / `FAFAFA_WAVE_B_EXAMPLES_REPORT_REL` 的隐式猜测。

**Architecture**
- 三个 consumer 统一输出 `linux_examples_selection`，候选值至少包括：`explicit_override`、`run_scoped_exact`、`static_same_run_fallback`、`run_scoped_missing`。
- 显式 `--linux-examples` 或 `FAFAFA_WAVE_B_EXAMPLES_REPORT_REL` 直接标记为 `explicit_override`；其余默认路径通过现有 resolver 决定并回填 selection。
- 这波只增强可观测性，不改变已有选择优先级和 strict 判定。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-examples-selection-observability.md`
- Add: `tests/scripts/test_wave_b_cross_platform_summary_examples_selection_explicit_override_contract.sh`
- Add: `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_examples_selection_explicit_override_contract.sh`
- Add: `tests/scripts/test_wave_b_b2_evidence_consistency_examples_selection_explicit_override_contract.sh`
- Modify: `scripts/generate_wave_b_cross_platform_summary.sh`
- Modify: `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- Modify: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 explicit override selection 合同并确认 RED。
2. 让三个 consumer 把 selection source 写入输出。
3. 跑 focused 合同和既有 default/run-id 回归，确认行为不变。
4. 回写 working memory 与当前汇总。

**Expected Outputs**
- reviewer 能直接从产物看出 Linux examples 是显式覆盖还是默认发现。
- explicit override 不再是“路径看起来对，但来源不可见”的黑箱。
- 下一波可以继续补 override warning/note contract，或只对 history retention 做轻量命名约束。
