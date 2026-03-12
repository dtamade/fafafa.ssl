# Wave B Cross Summary Run-ID Scoped Linux Default Plan

**Goal**
- 让 `scripts/generate_wave_b_cross_platform_summary.sh` 在只给 `--run-id` 时优先命中当前 run 的 Linux summary，而不是误捡 newer distractor run。
- 保持 cross-summary 的 checklist、Android extension、windows blocker layout 与 default output contract 不变。

**Architecture**
- 默认发现只收紧 `LINUX_SUMMARY`：先看 `wave_b_ci_gate_summary_${RUN_ID}.md`，缺失时才回退到最新 wildcard。
- 继续保持 `examples_compile_ci_gate.json` 的静态默认名不变；这波不扩大到 examples artifact 命名策略。
- helper 采用 Python `glob` + mtime 排序，避免 shell `ls -1t` 的路径/扩展脆弱性继续蔓延。
- 验证面由新 run-id scoped contract + 既有 cross-summary contracts + Wave B/TLS13 default reports contract 共同兜底。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-cross-summary-runid-scoped-linux-default.md`
- Add: `tests/scripts/test_wave_b_cross_platform_summary_run_id_scoped_linux_summary_contract.sh`
- Modify: `scripts/generate_wave_b_cross_platform_summary.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 direct cross-summary run-id scoped Linux summary 合同并确认 RED。
2. 给 cross-summary 补 run-id scoped `LINUX_SUMMARY` 默认解析。
3. 跑新合同与 cross-summary 既有 checklist/layout/default-output 回归。
4. 跑 Wave B/TLS13 default reports 合同、`bash -n` 与 `git diff --check`。
5. 回写 working memory、月度汇总与当前索引。

**Expected Outputs**
- direct cross-summary 在同目录存在 newer distractor Linux summary 时，不再把平台状态和 checklist 整体带偏。
- 既有 cross-summary 输出结构与 default output contract 继续保持绿色。
- 下一波可以继续收 `check_wave_b_b2_evidence_consistency.sh` 的 examples 默认发现与 TLS13 stale-fallback 边界。
