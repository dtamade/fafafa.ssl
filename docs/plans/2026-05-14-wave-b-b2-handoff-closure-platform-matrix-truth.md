# 2026-05-14 Wave B/B2 Handoff Closure Platform Matrix Truth

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 对 `closure_report` 平台状态表的盲信，避免表行缺失或平台状态非法时，顶层 handoff 仍继续给出 `CLOSED` / `READY_FOR_RUNNER` 一类正常状态。

## Architecture
- `prepare_wave_b_b2_handoff_bundle.sh` 现在已经开始校验：
  - `closure_status`
  - `consistency_status`
- 但它仍依赖 `closure_report` 的平台矩阵来生成：
  - `NEEDS_GATE_REPAIR`
  - Linux/macOS/Windows `Next Actions`
- 如果 `linux` / `macos` / `windows` 其中任一行缺失或状态非法，顶层平台判断就已经失真。

## Files
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_platform_matrix_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，伪造一个 `closure_status=CLOSED` 但平台状态表缺失 `windows` 行的 closure report。
2. 证明当前顶层 handoff bundle 仍可能把这条坏 closure matrix 当正常链处理。
3. 最小修改 `prepare_wave_b_b2_handoff_bundle.sh`：
   - 校验 `linux/macos/windows` 三个平台状态行是否齐全
   - 校验状态是否属于允许集合
   - 失败时落到 `NEEDS_REPORT_REPAIR`
4. 复跑 handoff / consistency 邻近回归。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh
bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_platform_matrix_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_platform_matrix_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh
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
- 新合同在修复前 FAIL：closure 平台状态矩阵不完整时，顶层 handoff 仍可能被错误当成正常链。
- 修复后：
  - 缺失/非法平台状态行落到 `NEEDS_REPORT_REPAIR`
  - 顶层报告显式写出缺失的平台状态信息
  - 已有 `closure_status` / `consistency_status` 校验与 handoff 语义保持兼容。
