# 2026-05-14 Wave B/B2 Handoff Linux Next Actions Truth

## Goal
收口 `scripts/prepare_wave_b_b2_handoff_bundle.sh` 顶层 `Next Actions` 的 Linux 缺口，避免 handoff bundle 在 Linux baseline 成为真实阻塞项时，仍只关注 macOS / Windows 与 Windows runtime companion，而把 Linux 从最终交接指引里漏掉。

## Architecture
- `prepare_wave_b_b2_handoff_bundle.sh` 是当前 Wave B/B2 交接链的顶层入口。
- 它已经会读取 `closure_report` 与 `consistency_report`，但旧的 `Next Actions` 只消费了 macOS / Windows platform state。
- 一旦 Linux 成为 closure 的当前阻塞项，handoff bundle 就会退化成只有抽象 replay 指令、没有具体 Linux 修复动作的半截指导。

## Files
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linux_next_actions_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，构造 Linux `FAIL`、macOS/Windows `PASS` 的 handoff bundle 场景。
2. 证明当前顶层 `Next Actions` 不提 Linux，只剩抽象 replay。
3. 最小修改 `prepare_wave_b_b2_handoff_bundle.sh`：
   - 从 `closure_report` 读取 Linux platform state
   - Linux 非 `PASS` 时显式给出 baseline 修复动作
   - 保持原有 replay command 与 handoff_state 逻辑不变
4. 复跑 handoff / cross-summary 邻近回归。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linux_next_actions_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_linux_next_actions_contract.sh
bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：handoff bundle `Next Actions` 不提 Linux `FAIL`。
- 修复后：
  - Linux 非 `PASS` 时 handoff bundle 显式提示修复或重跑 Linux baseline
  - macOS/Windows 已 `PASS` 时不再错误提示重跑它们
  - replay command 继续保留
- 邻近 handoff / cross-summary 合同保持 PASS。
