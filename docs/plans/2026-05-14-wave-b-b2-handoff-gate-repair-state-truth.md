# 2026-05-14 Wave B/B2 Handoff Gate Repair State Truth

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 的 `handoff_state` 语义漂移，避免已有平台 summary 已经明确 `FAIL/READY/DRY_RUN` 时，顶层交接状态仍错误落到 `READY_FOR_RUNNER`，把“需要修 gate”伪装成“只差 runner 证据”。

## Architecture
- `prepare_wave_b_b2_handoff_bundle.sh` 当前会汇总 closure / consistency / top-level replay command。
- `READY_FOR_RUNNER` 的真实语义应该是：
  - consistency 绿色
  - 但仍缺 runner 侧 summary/runtime evidence
- 只要已有平台 summary 已经存在且状态是 `FAIL/READY/DRY_RUN`，顶层就不该继续叫 `READY_FOR_RUNNER`。

## Files
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，构造 Linux `FAIL`、其余证据已齐的场景。
2. 证明当前顶层 bundle 仍给出 `handoff_state=READY_FOR_RUNNER`。
3. 最小修改 `prepare_wave_b_b2_handoff_bundle.sh`：
   - 提炼 gate-repair 状态判定
   - `FAIL/READY/DRY_RUN` 任一出现时落到新的 `NEEDS_GATE_REPAIR`
   - 保持 `NEEDS_EVIDENCE_SYNC`、`CLOSED`、`READY_FOR_RUNNER` 其余语义不变
4. 复跑 handoff 邻近回归。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh
bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linux_next_actions_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：Linux `FAIL` 时 handoff_state 仍是 `READY_FOR_RUNNER`。
- 修复后：
  - gate fail / malformed summary 场景落到 `NEEDS_GATE_REPAIR`
  - 缺 runner 证据但未失败的场景仍保持 `READY_FOR_RUNNER`
  - `NEEDS_EVIDENCE_SYNC` 与 `CLOSED` 语义保持不变。
