# Wave B B2 Closure/Evidence CLI Reports-Dir Passthrough Plan

**Goal**
- 给 `check_wave_b_b2_closure_readiness.sh` / `check_wave_b_b2_evidence_consistency.sh` 补齐 CLI `--reports-dir`，让 direct caller 不再必须依赖 env 才能切换报告目录。
- 保持现有 run-scoped default discovery、strict 语义与默认输出目录策略不变。

**Architecture**
- 两个脚本内部已经统一依赖 `REPORTS_DIR` 进行默认发现与默认输出，因此最小正确修复是把 `REPORTS_DIR` 的来源扩成 `CLI > env > default`。
- `prepare_wave_b_b2_handoff_bundle.sh` 已可通过显式 `--output-dir` 组织产物；这波只补 direct closure/evidence caller 的入口，不改 handoff fan-out 逻辑。
- 这波不改 linked evidence 规则，只让 caller-facing surface 更一致。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-b2-closure-evidence-cli-reports-dir-passthrough.md`
- Add: `tests/scripts/test_wave_b_b2_closure_evidence_cli_reports_dir_passthrough_contract.sh`
- Modify: `scripts/check_wave_b_b2_closure_readiness.sh`
- Modify: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 direct caller CLI passthrough 合同并确认 RED。
2. 给 closure / evidence 两个脚本补 `--reports-dir`。
3. 跑 focused 合同、既有 Wave B/TLS13 runtime 回归与 `bash -n`。
4. 回写 working memory 与当前汇总。

**Expected Outputs**
- direct caller 可以只靠 CLI 切换 Wave B B2 closure/evidence 的 reports dir。
- closure/evidence 的默认发现会跟随 CLI reports-dir 走。
- 既有 examples selection / warning / run-id consistency 与 Wave B/TLS13 runtime 合同继续保持绿色。
