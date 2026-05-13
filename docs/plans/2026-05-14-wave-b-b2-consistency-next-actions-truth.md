# 2026-05-14 Wave B/B2 Consistency Next Actions Truth

## Goal
收口 `scripts/check_wave_b_b2_evidence_consistency.sh` 报告面的操作指引漂移，避免它在 `closure_status_note=IN_PROGRESS` 时只显示 `CONSISTENT + Gate Rule`，却不说明 handoff 仍未闭环，也不把操作者指回当前单一的 `prepare_wave_b_b2_handoff_bundle.sh` 刷新入口。

## Architecture
- `check_wave_b_b2_evidence_consistency.sh` 当前负责 Wave B/B2 证据一致性层的状态输出。
- 上一批已经补齐 `closure_report` 元数据校验，但报告正文仍缺 `Next Actions`。
- 真实 handoff 刷新单一入口已经收敛到 `scripts/prepare_wave_b_b2_handoff_bundle.sh`，因此 consistency 报告不能继续停留在“有状态、无指引”的半截面。

## Files
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，构造 Linux-only / `closure_status_note=IN_PROGRESS` 的真实场景。
2. 证明当前 consistency 报告没有 `Next Actions`，也没有解释这不等于 handoff 已闭环。
3. 最小修改 `check_wave_b_b2_evidence_consistency.sh`：
   - 基于 `consistency_status + closure_status_note` 生成 `Next Actions`
   - 在 `IN_PROGRESS` 时明确说明 handoff 尚未闭环
   - 指回 `scripts/prepare_wave_b_b2_handoff_bundle.sh` 作为当前刷新入口
4. 复跑 focused + handoff/workflow 邻近回归。
5. 更新三份 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh
bash tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh
bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：consistency 报告缺少 `## Next Actions`。
- 修复后：
  - 报告包含 `## Next Actions`
  - 在 `closure_status_note=IN_PROGRESS` 时显式说明 handoff 尚未闭环
  - 指向 `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- 邻近 handoff/workflow contracts 继续 PASS。
