# Wave B B2 Handoff Run-ID Scoped Linux Summary Plan

**Goal**
- 让 `scripts/prepare_wave_b_b2_handoff_bundle.sh` 在只给 `--run-id` 时优先命中当前 run 的 Linux summary，而不是误捡 newer distractor run。
- 保持 handoff bundle 既有 consistency snapshot、closure semantics、windows blocker visibility 与 default reports contract 不变。

**Architecture**
- handoff bundle 默认发现只收紧 `LINUX_SUMMARY`：先看 `wave_b_ci_gate_summary_${RUN_ID}.md`，缺失时才回退到最新 wildcard。
- 继续保持 `examples_compile_ci_gate.json` 的静态默认名不变；这波不扩大到 examples artifact 命名策略。
- 解析 helper 复用 Python `glob` + mtime 排序，避免 shell `ls -1t` 在复杂路径或后续扩展时继续漂移。
- 验证面由新 handoff run-id scoped contract + 既有 consistency snapshot / closure semantics / windows blocker visibility / Wave B-TLS13 default reports contract 共同兜底。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-b2-handoff-runid-scoped-linux-summary.md`
- Add: `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_run_id_scoped_linux_summary_contract.sh`
- Modify: `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 handoff bundle run-id scoped Linux summary 合同并确认 RED。
2. 给 handoff bundle 补 run-id scoped `LINUX_SUMMARY` 默认解析。
3. 跑新合同与 handoff 既有 snapshot/semantics/blocker 可见性回归。
4. 跑 Wave B/TLS13 default reports 合同、`bash -n` 与 `git diff --check`。
5. 回写 working memory、月度汇总与当前索引。

**Expected Outputs**
- handoff bundle 在同目录存在 newer distractor Linux summary 时，不再把 cross summary / closure / evidence 链整体带偏。
- 既有 handoff 输出结构与默认 reports-dir contract 继续保持绿色。
- 下一波可以把同型 run-id scoped 默认发现继续下沉到 direct `generate_wave_b_cross_platform_summary.sh`，而不是反复回修 handoff bundle。
