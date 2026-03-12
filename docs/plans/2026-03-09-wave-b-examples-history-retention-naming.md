# Wave B Examples History Retention Naming Plan

**Goal**
- 给 `examples-compile-history/` 补一个轻量 retention contract，只约束 archive copy 的命名与 generic alias 禁入。
- 不做历史清理，不扩展 consumer 语义。

**Architecture**
- 当前 producer 已经默认产出 `current alias + run-scoped copy + archive copy`，并会清理 history bucket 中遗留的 generic alias。
- 但 archive copy 路径仍允许通过 env 被配置成任意文件名；一旦被误设成 `examples_compile_ci_gate.json` / `examples_compile_latest.json` 之类的 generic 名称，就会破坏“history bucket 只保留 run-scoped history”的治理边界。
- 最小正确修复是把 archive copy 文件名规范化为 `examples_compile_ci_gate_<run_id>.json`；目录仍允许自定义，但命名不再漂移。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-examples-history-retention-naming.md`
- Add: `tests/scripts/test_wave_b_ci_gate_examples_history_retention_naming_contract.sh`
- Modify: `scripts/run_wave_b_ci_gate.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 history retention naming 合同并确认 RED。
2. 把 archive copy 文件名规范化为 run-scoped 命名。
3. 跑 focused 合同与既有 archive/runtime 回归。
4. 回写 working memory 与下一波建议。

**Expected Outputs**
- history bucket 中 archive copy 始终采用 `examples_compile_ci_gate_<run_id>.json`。
- generic alias 继续只作为 cleanup 目标，不会成为 archive copy 本体。
- caller 仍可自定义 archive 目录，但不能把 archive copy 命名漂移成 generic 名称。
