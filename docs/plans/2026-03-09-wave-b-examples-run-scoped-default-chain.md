# Wave B Examples Run-Scoped Default Chain Plan

**Goal**
- 让 Wave B 的 examples 证据链同时保留静态 `examples_compile_ci_gate.json` 和当前 run 专属副本 `examples_compile_ci_gate_<run_id>.json`。
- 让 `cross-summary`、`handoff bundle`、`evidence checker` 默认优先读取当前 run 专属副本，再回退到静态默认文件。

**Architecture**
- `run_wave_b_ci_gate.sh` 继续把主输出写到静态默认 `examples_compile_ci_gate.json`，并额外复制一份到 `examples_compile_ci_gate_<run_id>.json`，兼顾兼容性与可追踪性。
- `generate_wave_b_cross_platform_summary.sh`、`prepare_wave_b_b2_handoff_bundle.sh`、`check_wave_b_b2_evidence_consistency.sh` 的默认 `linux examples` 解析改成：`run-scoped exact -> static fallback`。
- 显式 `--linux-examples` 或 `FAFAFA_WAVE_B_EXAMPLES_REPORT_REL` 仍然优先，不破坏现有手工注入路径。
- 这波不改静态默认文件名，也不引入 wildcard 搜索；只补当前 run 专属副本与明确的两段式默认发现。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-examples-run-scoped-default-chain.md`
- Add: `tests/scripts/test_wave_b_ci_gate_examples_report_run_scoped_alias_contract.sh`
- Add: `tests/scripts/test_wave_b_cross_platform_summary_default_examples_run_scoped_contract.sh`
- Add: `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_default_examples_run_scoped_contract.sh`
- Modify: `scripts/run_wave_b_ci_gate.sh`
- Modify: `scripts/generate_wave_b_cross_platform_summary.sh`
- Modify: `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- Modify: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 producer/cross-summary/handoff 三条 focused 合同并确认 RED。
2. 让 gate 额外落 run-scoped examples 副本。
3. 让三个 consumer 默认优先读 run-scoped、副本缺失时回退 static。
4. 跑 focused 合同、既有 gate/cross-summary/evidence/default-reports 回归。
5. 回写 working memory、月度汇总与当前索引。

**Expected Outputs**
- 默认 examples 静态路径不再是唯一真相，当前 run 有可直接命中的专属副本。
- cross-summary / handoff / evidence 默认链路不再被 stale static examples 报告轻易带偏。
- 下一波可以继续收 `evidence checker` 的缺失语义，或考虑是否为静态 examples 文件增加 archive/current alias 治理约束。
