# Wave B B2 Evidence Examples Run-ID Consistency Plan

**Goal**
- 让 `scripts/check_wave_b_b2_evidence_consistency.sh` 在 Linux examples JSON 显式携带 `run_id` 且与目标 run 不一致时，稳定判为不一致。
- 保持 legacy examples JSON（没有 `run_id` 字段）继续按“JSON 有效即可”兼容，不打破既有 blocker linkage / closure / default reports contract。

**Architecture**
- `check_json_artifact()` 继续负责 JSON 证据，不把 markdown run-id 解析逻辑硬塞进 JSON 分支。
- 新增 `parse_run_id_json()`，只从 top-level `run_id`、`metadata.run_id`、`summary.run_id` 三个轻量位置提取 run-id；不扩成通用 schema 校验器。
- JSON 非法时仍按 mismatch 处理；JSON 合法但未暴露 `run_id` 时保持 `json_valid=YES`、`run_id_match=n/a` 的 backward-compatible 语义。
- 验证面用新 examples run-id contract 加上既有 windows blocker linkage、closure run-id scoped default、Wave B/TLS13 default reports contract 共同兜底。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-b2-evidence-examples-runid-consistency.md`
- Add: `tests/scripts/test_wave_b_b2_evidence_consistency_examples_runid_contract.sh`
- Modify: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 examples JSON run-id mismatch 合同并确认 strict-mode RED。
2. 给 evidence checker 补可选 JSON run-id 解析与 mismatch 判定。
3. 跑新合同、windows blocker linkage、closure run-id scoped default、Wave B/TLS13 default reports 回归。
4. 跑 `bash -n` 与 `git diff --check`。
5. 回写 working memory、当前索引与月度汇总。

**Expected Outputs**
- examples JSON 不再只校验“是不是合法 JSON”，而会在可观测到 `run_id` 漂移时及时拉红。
- legacy 无 `run_id` JSON 仍可被 evidence checker 接受，避免把旧报告历史一口气打碎。
- 下一波可以继续看 evidence checker 默认发现 / stale artifact fallback，而不是再回头修 examples run-id 漂移。
