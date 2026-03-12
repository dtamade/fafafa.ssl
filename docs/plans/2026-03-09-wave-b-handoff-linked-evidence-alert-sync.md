# Wave B Handoff Linked Evidence Alert Sync Plan

**Goal**
- 让 `prepare_wave_b_b2_handoff_bundle.sh` 的顶部 `Consistency Alert Summary` 计数面板感知 `linked_evidence_mismatch`。
- 避免 handoff bundle 出现 `consistency_status=INCONSISTENT` 但 `alert_state=CLEAR` 的自相矛盾状态。

**Architecture**
- evidence checker 新增了 `linked_evidence_mismatch` 指标，但 handoff 仍只读取 `required_missing` 与 `runid_mismatch_or_parse_issue`。
- 因此只要 inconsistency 完全来自 linked evidence，handoff 虽然会把整体状态切成 `NEEDS_EVIDENCE_SYNC`，顶部告警面板却仍显示 `CLEAR`。
- 最小正确修复是：读取并展示 `linked_evidence_mismatch`，并把它纳入 `alert_state` 的 WARN 判定。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-handoff-linked-evidence-alert-sync.md`
- Add: `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linked_evidence_alert_sync_contract.sh`
- Modify: `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 linked-evidence alert sync 合同并确认 RED。
2. 让 handoff 读取/展示 `linked_evidence_mismatch` 并纳入告警态。
3. 跑 focused 合同与 handoff/runtime 回归。
4. 回写 working memory 与下一波建议。

**Expected Outputs**
- handoff bundle 会显示 `- linked_evidence_mismatch: N`。
- 只要 `linked_evidence_mismatch > 0`，`alert_state` 就是 `WARN`。
- 既有 `required_missing` / `runid_mismatch_or_parse_issue` 语义保持不变。
