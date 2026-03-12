# Wave B Evidence Cross-Summary Linked Examples Sync Plan

**Goal**
- 收紧 `check_wave_b_b2_evidence_consistency.sh` 对 `cross_summary` 内嵌 `linux_examples_json / linux_examples_selection` 的 linked-evidence 一致性校验。
- 避免 `cross_summary` 虽然同 run，但内部引用的 Linux examples 仍指向错误 alias/selection 时被误判为 `CONSISTENT`。

**Architecture**
- 当前 evidence checker 只校验 artifact 自身是否存在、是否能解析 `run_id`、以及 `run_id` 是否匹配。
- 这意味着 `cross_summary` 只要文件存在且 `run_id` 正确，即使它里面声明的 `linux_examples_json` / `linux_examples_selection` 与 evidence checker 当前实际选中的证据不一致，也会被当成 `ok`。
- 最小正确修复是给 `cross_summary` 增加一层 linked-evidence 校验：仅在字段存在时做比对，不强制旧版最小 fixture 必须携带这些字段。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-evidence-cross-summary-linked-examples-sync.md`
- Add: `tests/scripts/test_wave_b_b2_evidence_consistency_cross_summary_linked_examples_contract.sh`
- Modify: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 cross-summary linked examples mismatch 合同并确认 RED。
2. 给 evidence checker 增加 linked-evidence 比对与报告输出。
3. 跑 focused 合同与既有 Wave B handoff/runtime 回归。
4. 回写 working memory 与下一波建议。

**Expected Outputs**
- `cross_summary` 即使 `run_id` 正确，只要其 `linux_examples_json` / `linux_examples_selection` 与当前 evidence 解析结果不一致，也会被判为 `INCONSISTENT`。
- 旧版 minimal fixture 若未提供这些字段，保持现状，不被强制判错。
- 报告里能直接看出 mismatch 落在 `cross_summary` 的 linked examples 语义上。
