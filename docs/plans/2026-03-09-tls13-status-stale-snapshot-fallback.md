# TLS13 Status Stale Snapshot Fallback Plan

**Goal**
- 让 `scripts/export_tls13_signer_gate_status_json.sh` 在目标 run 的 snapshot 缺失时，不再误吃其他 run 的 stale snapshot。
- 保持 run-id scoped default selection 与 Wave B/TLS13 default reports contract 不变。

**Architecture**
- `summary` 与 `bench_json` 仍保持 `run_id exact -> latest fallback`，因为它们是 status export 的 source evidence。
- `snapshot` 是派生工件，跨 run fallback 会直接污染目标 run 的健康态；因此默认解析改成“只接受当前 run exact snapshot，否则视为 missing”。
- 这波不把 status export 扩成自动生成 snapshot 的 orchestrator；缺 snapshot 时保持 `MISSING/ATTENTION` 更安全。
- 验证面由新 stale snapshot contract + 既有 run-id scoped default selection contract + Wave B/TLS13 default reports runtime contract 共同兜底。

**Files**
- Add: `docs/plans/2026-03-09-tls13-status-stale-snapshot-fallback.md`
- Add: `tests/scripts/test_tls13_signer_gate_status_stale_snapshot_fallback_contract.sh`
- Modify: `scripts/export_tls13_signer_gate_status_json.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 stale snapshot fallback 合同并确认 RED。
2. 仅收紧 status export 的 default snapshot 解析，不改 summary/bench 默认发现。
3. 跑新合同、run-id scoped default selection、Wave B/TLS13 default reports 回归。
4. 跑 `bash -n` 与 `git diff --check`。
5. 回写 working memory、月度汇总与当前索引。

**Expected Outputs**
- status export 在目标 run 缺 snapshot 时不再引用其他 run 的 stale snapshot。
- target run 仍会保留自己的 summary/bench 证据；snapshot 缺失时安全降级为 `MISSING/ATTENTION`。
- 下一波可以继续看 `check_wave_b_b2_evidence_consistency.sh` 的 examples 默认发现或 `generate_tls13_signer_gate_snapshot.sh` 的 history fallback。
