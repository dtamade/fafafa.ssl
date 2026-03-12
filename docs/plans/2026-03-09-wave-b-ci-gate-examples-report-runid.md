# Wave B CI Gate Examples Report Run-ID Plan

**Goal**
- 让 `scripts/run_wave_b_ci_gate.sh` 产出的 `examples_compile_ci_gate.json` 明确携带所属 `run_id`。
- 让 `scripts/check_wave_b_b2_evidence_consistency.sh` 对默认静态 examples 路径也能借助 `run_id` 做真实的一致性判定，而不是长期依赖“无 run_id 也兼容”。

**Architecture**
- `run_wave_b_ci_gate.sh` 在确定 `RUN_ID` 后显式导出 `FAFAFA_WAVE_B_CI_GATE_RUN_ID`，确保子脚本拿到同一 run identity。
- `verify_examples_compile.sh` 在 JSON 输出里可选写出 `run_id`，来源优先读 `FAFAFA_EXAMPLES_RUN_ID`，其次读 `FAFAFA_WAVE_B_CI_GATE_RUN_ID`。
- 这波不改 examples report 的默认文件名；仍然使用静态 `examples_compile_ci_gate.json`，但通过可观测的 `run_id` 降低默认静态路径带来的串味风险。
- 验证面由新的 gate examples run-id contract + 既有 Wave B CI gate contracts + B2 evidence examples run-id contract + Wave B/TLS13 default reports runtime contract 共同兜底。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-ci-gate-examples-report-runid.md`
- Add: `tests/scripts/test_wave_b_ci_gate_examples_report_runid_contract.sh`
- Modify: `scripts/run_wave_b_ci_gate.sh`
- Modify: `scripts/verify_examples_compile.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 gate examples report run-id 合同并确认 RED。
2. 导出 Wave B gate `RUN_ID`，并让 examples JSON 写出 `run_id`。
3. 跑新合同、Wave B CI gate quote/isolation/FPC passthrough 回归。
4. 跑 B2 evidence examples run-id 与 Wave B/TLS13 default reports 回归。
5. 回写 working memory、月度汇总与当前索引。

**Expected Outputs**
- 默认 `examples_compile_ci_gate.json` 不再是“无主报告”，能明确标出所属 run。
- evidence checker 在静态默认 examples 路径上也能更可靠地发现 stale report。
- 下一波可以更有把握地审 `check_wave_b_b2_evidence_consistency.sh` 的 default examples artifact 发现策略，而不是继续补盲。
