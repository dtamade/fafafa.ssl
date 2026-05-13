# 2026-05-14 Wave B/B2 Closure Linux Next Actions Truth

## Goal
收口 `scripts/check_wave_b_b2_closure_readiness.sh` 的 `Next Actions` 与当前 Linux baseline 必需前提之间的漂移，避免 closure 报告在 Linux 为 `READY/FAIL/PENDING` 时仍只提醒 macOS/Windows，而把最关键的 Linux 修复动作藏掉。

## Architecture
- `check_wave_b_b2_closure_readiness.sh` 负责三平台 summary 闭环判定。
- Linux baseline 已经是 Wave B/B2 handoff 的必需前提，不能再被当成“默认没问题”的隐形背景。
- 现有 closure 脚本虽能把 Linux 判成 `READY` / `FAIL` / `PENDING`，但 `Next Actions` 仍是静态文案，和当前状态矩阵脱节。

## Files
- `scripts/check_wave_b_b2_closure_readiness.sh`
- `tests/scripts/test_wave_b_b2_closure_linux_next_actions_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，构造 Linux summary 缺 `Overall Status` 的 `READY` 场景。
2. 证明当前 closure 报告虽然显示 Linux `READY`，但 `Next Actions` 没有任何 Linux 指引。
3. 最小修改 `check_wave_b_b2_closure_readiness.sh`：
   - 基于 linux/macos/windows state 动态生成 `Next Actions`
   - Linux 非 `PASS` 时显式要求修复或重跑 baseline
   - 继续保留 prepare 作为最终刷新入口
4. 复跑 closure/handoff 邻近回归。
5. 更新三份 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_b2_closure_linux_next_actions_contract.sh
bash tests/scripts/test_wave_b_b2_closure_linux_next_actions_contract.sh
bash -n scripts/check_wave_b_b2_closure_readiness.sh
bash tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：closure 报告不提 Linux `READY/FAIL/PENDING`。
- 修复后：
  - Linux 非 `PASS` 时 `Next Actions` 明确提到 Linux
  - 仍保持 `prepare_wave_b_b2_handoff_bundle.sh` 作为最终刷新入口
- 邻近 closure/handoff 合同保持 PASS。
