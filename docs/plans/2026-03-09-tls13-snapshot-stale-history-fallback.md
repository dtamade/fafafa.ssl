# TLS13 Snapshot Stale History Fallback Plan

**Goal**
- 让 `scripts/generate_tls13_signer_gate_snapshot.sh` 在目标 run 缺 history 时，不再误吃其他 run 的 stale history。
- 保持 run-id scoped default selection、status stale snapshot fallback 与 Wave B/TLS13 default reports contract 不变。

**Architecture**
- `summary` 与 `bench_json` 仍保持 `run_id exact -> latest fallback`，因为它们是 snapshot 的 source evidence。
- `history` 是派生历史工件，跨 run fallback 只会污染 evidence 路径，不该改变目标 run 的快照；因此默认解析改成 exact-only。
- 这波不把 snapshot 生成器扩成 history 自动重建器；缺 history 时显示 `<none>` 更符合当前职责边界。
- 验证面由新 stale history contract + 既有 run-id scoped default selection + status stale snapshot fallback + Wave B/TLS13 default reports runtime contract 共同兜底。

**Files**
- Add: `docs/plans/2026-03-09-tls13-snapshot-stale-history-fallback.md`
- Add: `tests/scripts/test_tls13_signer_gate_snapshot_stale_history_fallback_contract.sh`
- Modify: `scripts/generate_tls13_signer_gate_snapshot.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 stale history fallback 合同并确认 RED。
2. 仅收紧 snapshot 的 default history 解析，不改 summary/bench 默认发现。
3. 跑新合同、run-id scoped default selection、status stale snapshot fallback、Wave B/TLS13 default reports 回归。
4. 跑 `bash -n` 与 `git diff --check`。
5. 回写 working memory、月度汇总与当前索引。

**Expected Outputs**
- snapshot 在目标 run 缺 history 时不再引用其他 run 的 stale history。
- target run 的 summary/bench 仍正常保留；history 缺失时安全显示 `<none>`。
- 下一波可以继续处理 `examples JSON` 生产侧的 `run_id` 可观测性，或继续做 TLS13 其他 fallback 边界。
