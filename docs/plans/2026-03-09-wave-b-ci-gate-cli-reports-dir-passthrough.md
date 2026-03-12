# Wave B CI Gate CLI Reports-Dir Passthrough Plan

**Goal**
- 给 `run_wave_b_ci_gate.sh` 补齐 caller-facing `--reports-dir`，让 direct caller 可以不靠 env 就切换默认 reports 根目录。
- 保持显式 `--examples-report`、`--summary-out`、`--tls13-sign-bench-json-out` 的覆盖语义不变。

**Architecture**
- `run_wave_b_ci_gate.sh` 早已把 `REPORTS_DIR` 作为默认输出根：examples JSON、summary、TLS13 bench JSON 与各 step log 都会从它派生。
- 但当前 `REPORTS_DIR` 只有 env 入口 `FAFAFA_WAVE_B_REPORTS_DIR`，direct caller 无法像其他 Wave B / TLS13 脚本那样直接传 CLI `--reports-dir`。
- 最小正确修复是只补参数解析与 usage 文案；默认派生链本身已经存在，不需要再改业务判定或 fallback 规则。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-ci-gate-cli-reports-dir-passthrough.md`
- Add: `tests/scripts/test_wave_b_ci_gate_cli_reports_dir_passthrough_contract.sh`
- Modify: `scripts/run_wave_b_ci_gate.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 direct caller `--reports-dir` 合同并确认 RED。
2. 给 `run_wave_b_ci_gate.sh` 增加 `--reports-dir` 参数解析与帮助文案。
3. 跑 focused 合同与既有 Wave B/TLS13 runtime 回归。
4. 回写 working memory 与当前汇总。

**Expected Outputs**
- direct caller 只给 `--reports-dir` 时，Wave B CI gate 会把默认 examples JSON、summary、archive/run-scoped 副本与 step logs 全部写到该目录链下。
- caller 不再需要为了切目录而额外注入 `FAFAFA_WAVE_B_REPORTS_DIR`。
- 显式 `--examples-report` / `--summary-out` / `--tls13-sign-bench-json-out` 继续拥有更高优先级。
