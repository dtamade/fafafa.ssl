# 2026-05-14 Wave B/B2 Consistency Closure Status Parse Truth

## Goal
收口 `scripts/check_wave_b_b2_evidence_consistency.sh` 对 malformed `closure_report` 的假绿灯，避免 closure 报告缺失 `closure_status` 元数据时，`consistency` 仍输出 `CONSISTENT` 并把顶层 `closure_status_note` 留空。

## Architecture
- `check_wave_b_b2_evidence_consistency.sh` 负责产出 Wave B/B2 证据一致性 markdown，并通过 `runid_mismatch_or_parse_issue` 汇总解析类错误。
- `closure_report` 目前是 required artifact，但旧逻辑只检查 run_id，不检查 `closure_status` 是否存在/合法。
- `prepare_wave_b_b2_handoff_bundle.sh` 上层会继续消费 `closure_status` / `consistency_status`，因此底层 consistency 不能把坏掉的 closure 证据伪装成绿色。

## Files
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，构造 run_id 正常但 `closure.md` 缺少 `closure_status` 行的场景。
2. 运行合同，确认当前 strict 模式仍错误返回 0，且报告是 `CONSISTENT`。
3. 最小修改 `check_wave_b_b2_evidence_consistency.sh`：
   - 为 `closure_report` 增加专门校验
   - 要求 `closure_status` 必须存在且属于 `IN_PROGRESS` / `CLOSED`
   - 缺失或非法时计入 `runid_mismatch_or_parse_issue`，并把报告 note 写明
4. 复跑 focused + 邻近 handoff 回归。
5. 更新 `task_plan.md` / `findings.md` / `progress.md`，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh
bash tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh
bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：strict 不应接受缺少 `closure_status` 的 `closure_report`。
- 修复后：
  - `consistency_status: **INCONSISTENT**`
  - `runid_mismatch_or_parse_issue: 1`
  - `closure_status_note: closure_status missing`
  - `closure_report` 行 note 变为 `closure_status missing`
- 邻近 handoff / workflow contracts 保持 PASS。
