# Wave B Examples Current Alias And Archive Governance Plan

**Goal**
- 给 `examples_compile_ci_gate.json` 建立清晰的治理边界：谁是 current alias、谁是 run-scoped truth、谁负责 archive history。
- 让 producer 自己落下 archive copy，并清理 history 目录里可能误导 consumer 的静态 alias 噪音。

**Architecture**
- `run_wave_b_ci_gate.sh` 继续保留顶层 static alias `examples_compile_ci_gate.json` 作为 current alias，并保留顶层 run-scoped copy `examples_compile_ci_gate_<run_id>.json`。
- 新增同目录 history bucket：`<reports_dir>/examples-compile-history/examples_compile_ci_gate_<run_id>.json`，作为 archive copy。
- history bucket 不保留静态 alias；若存在旧的 `examples-compile-history/examples_compile_ci_gate.json`，producer 在当前 run 写 archive 时主动清理。
- summary 明确暴露 current alias、alias owner run_id、run-scoped copy、archive copy、history alias cleanup 结果，避免“当前真相”继续靠约定推断。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-examples-current-alias-archive-governance.md`
- Add: `tests/scripts/test_wave_b_ci_gate_examples_archive_governance_contract.sh`
- Add: `tests/scripts/test_wave_b_ci_gate_examples_history_alias_cleanup_contract.sh`
- Modify: `scripts/run_wave_b_ci_gate.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 archive governance / stale history alias cleanup 合同，并确认 RED。
2. 让 producer 在 examples 成功产物存在时落 archive copy，并清理 history 静态 alias。
3. 在 summary 输出 current alias owner / run-scoped / archive / cleanup 元数据。
4. 跑 focused 合同和既有 examples producer 回归。
5. 回写 working memory 与当前汇总。

**Expected Outputs**
- `run_wave_b_ci_gate.sh` 自己定义并产出 `current alias + run-scoped + archive history` 三层语义。
- history bucket 不再残留 generic static alias，降低后续消费方误读风险。
- 下一波可以继续 contract 化显式 `--examples-report` / env override 的 owner/warning 语义，或单开旧 history 清理批次。
