# Wave B Examples Static Fallback Run-ID Guard Plan

**Goal**
- 阻止 stale 的静态 `examples_compile_ci_gate.json` 在默认链路里冒充当前 run 的 Linux examples 证据。
- 保留兼容性：当 run-scoped 副本缺失但静态 alias 的 `run_id` 仍属于当前 run 时，默认 consumer 仍可安全回退。

**Architecture**
- `generate_wave_b_cross_platform_summary.sh`、`prepare_wave_b_b2_handoff_bundle.sh`、`check_wave_b_b2_evidence_consistency.sh` 的默认 examples 解析从“按存在性回退”收紧为“按 run_id 安全回退”。
- 默认顺序改成：显式 `--linux-examples` / `FAFAFA_WAVE_B_EXAMPLES_REPORT_REL` 优先；否则 `run-scoped exact`；若缺失再检查静态 alias，只有其 JSON `run_id == RUN_ID` 才允许回退；否则仍回到 run-scoped 预期路径并按 missing 处理。
- 这波不改 producer，不改静态 alias 文件名，也不做历史清理；只收默认发现语义，避免 stale 静态文件继续串味。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-examples-static-fallback-runid-guard.md`
- Add: `tests/scripts/test_wave_b_b2_evidence_consistency_default_examples_stale_static_guard_contract.sh`
- Add: `tests/scripts/test_wave_b_cross_platform_summary_default_examples_stale_static_guard_contract.sh`
- Add: `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_default_examples_stale_static_guard_contract.sh`
- Modify: `scripts/generate_wave_b_cross_platform_summary.sh`
- Modify: `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- Modify: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 stale static guard 合同，并观察默认 fallback 误选静态 alias 的 RED。
2. 让三个 default consumer 只在静态 alias `run_id` 命中当前 run 时才回退。
3. 跑 focused 合同与已有 run-scoped/default 回归，确认兼容链未退化。
4. 回写 working memory、月度汇总与当前索引。

**Expected Outputs**
- run-scoped 缺失而 static stale 仍在时，默认 consumer 不再误消费旧 run 的 examples JSON。
- evidence checker 会把这类情况报告成“当前 run 缺失证据”，而不是“默认吃到旧文件后再 mismatch”。
- 下一波可以继续给静态 alias 增加 archive/current 治理 contract，或单开历史 `test-reports` 清理。
