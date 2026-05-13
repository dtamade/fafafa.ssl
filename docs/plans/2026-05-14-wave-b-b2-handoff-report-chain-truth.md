# 2026-05-14 Wave B/B2 Handoff Report Chain Truth

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 对下游报告元数据的盲信，避免 `closure_report` / `consistency_report` 缺失或写坏关键状态字段时，顶层交接仍继续给出普通 `handoff_state`，把“坏报告链”伪装成“还能继续 runner/handoff”。

## Architecture
- `prepare_wave_b_b2_handoff_bundle.sh` 是当前 Wave B/B2 顶层单一入口。
- 它会先生成：
  - cross summary
  - closure readiness
  - evidence consistency
  - handoff bundle
- 顶层 handoff 目前依赖两个下游字段决定状态语义：
  - `closure_status`
  - `consistency_status`
- 如果这两个字段缺失或非法，顶层就不该继续落到 `READY_FOR_RUNNER` / `CLOSED` / `NEEDS_EVIDENCE_SYNC` 这种“正常链路状态”。

## Files
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，伪造一个缺失 `consistency_status` 的 downstream consistency report。
2. 证明当前顶层 handoff bundle 仍把这条坏 report chain 当正常链继续消费。
3. 最小修改 `prepare_wave_b_b2_handoff_bundle.sh`：
   - 提炼 `closure_status` / `consistency_status` 解析函数
   - 校验允许值
   - 缺失/非法时落到 `NEEDS_REPORT_REPAIR`
   - 报告里显式写出 `report_chain_note`
4. 复跑 handoff 与 consistency 邻近回归。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh
bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linux_next_actions_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh
bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：顶层 handoff bundle 继续把缺失 `consistency_status` 的坏报告链当正常链处理。
- 修复后：
  - malformed downstream report metadata 明确落到 `NEEDS_REPORT_REPAIR`
  - 顶层报告显式输出 `report_chain_note`
  - 旧的 `READY_FOR_RUNNER` / `CLOSED` / `NEEDS_EVIDENCE_SYNC` 语义在正常链路下保持不变。
