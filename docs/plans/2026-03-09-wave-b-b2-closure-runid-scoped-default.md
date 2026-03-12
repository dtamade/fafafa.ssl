# Wave B B2 Closure Run-ID Scoped Default Plan

**Goal**
- 让 `scripts/check_wave_b_b2_closure_readiness.sh` 在只给 `--run-id` 时优先选择当前 run 的 Linux summary。
- 保持 macOS / Windows 显式传参语义与既有 dry-run / skipped contract 不变。

**Architecture**
- `LINUX_SUMMARY` 默认解析先看 `wave_b_ci_gate_summary_${RUN_ID}.md`，存在则直接使用；只有缺失时才回退到最新 wildcard 匹配。
- 这波不改 macOS / Windows summary 的“显式传参优先”边界，也不把 closure checker 扩成更重的自动发现器。
- 解析 helper 继续使用 Python `glob` + mtime 排序，避免 shell glob 在复杂路径下继续漂移。
- 验证面由新 run-id scoped contract + 既有 dryrun/skipped semantics contract + Wave B/TLS13 default reports contract 共同兜底。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-b2-closure-runid-scoped-default.md`
- Add: `tests/scripts/test_wave_b_b2_closure_readiness_run_id_scoped_default_contract.sh`
- Modify: `scripts/check_wave_b_b2_closure_readiness.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 run-id scoped 默认选择合同，确认 RED。
2. 给 closure checker 补 run-id scoped Linux summary 默认解析。
3. 跑新合同、dryrun/skipped 语义合同、Wave B/TLS13 default reports 合同。
4. 跑 evidence linkage 防回归与 `git diff --check`。
5. 回写 working memory 与当前汇总。

**Expected Outputs**
- closure checker 不再因同目录下 newer distractor run 而误判当前 run 未闭环。
- 既有 dryrun/skipped 与 evidence linkage 语义保持不变。
- 下一波可以继续收 `check_wave_b_b2_evidence_consistency.sh` 的默认发现与 linkage 细节，而不是继续回修 closure。
