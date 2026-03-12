# Wave B CI Gate Mixed Output Priority Plan

**Goal**
- 收紧 `run_wave_b_ci_gate.sh` 在 `--reports-dir` 与显式 `--examples-report` / `--summary-out` / `--tls13-sign-bench-json-out` 组合输入下的输出优先级语义。
- 让显式输出路径在父目录不存在时也能稳定落盘，不要求 caller 预先 `mkdir -p`。

**Architecture**
- `run_wave_b_ci_gate.sh` 已有 `CLI > env > default` 的路径解析顺序，但当前只会创建 `REPORTS_DIR` 自身。
- 当 caller 把 `--examples-report` 或 `--summary-out` 指到独立子目录时，父目录如果不存在，落盘会失败或静默丢失。
- 更稳的做法是：保持优先级语义不变，同时在 producer / orchestrator 侧为显式输出路径补父目录创建；其中 examples JSON 的根因在 `verify_examples_compile.sh` 自身。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-ci-gate-mixed-output-priority.md`
- Add: `tests/scripts/test_wave_b_ci_gate_mixed_output_priority_contract.sh`
- Modify: `scripts/run_wave_b_ci_gate.sh`
- Modify: `scripts/verify_examples_compile.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 mixed-output priority 合同并确认 RED。
2. 修复显式输出路径父目录创建问题。
3. 跑 focused 合同与既有 Wave B/TLS13 回归。
4. 回写 working memory 与下一波建议。

**Expected Outputs**
- caller 同时传 `--reports-dir` 和显式输出参数时，显式路径继续优先。
- logs / run-scoped / archive 仍按 `REPORTS_DIR` 归档。
- caller 不需要预创建显式输出路径的父目录。
