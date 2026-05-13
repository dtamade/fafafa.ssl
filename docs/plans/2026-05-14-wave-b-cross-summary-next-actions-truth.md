# 2026-05-14 Wave B Cross Summary Next Actions Truth

## Goal
收口 `scripts/generate_wave_b_cross_platform_summary.sh` 的 `Next Actions` 漂移，避免 cross summary 在 Linux baseline 已成为必需前提后，仍使用固定 macOS/Windows 模板，并继续把操作者引向“重新运行本脚本”这一条只刷新局部摘要的旧入口。

## Architecture
- `generate_wave_b_cross_platform_summary.sh` 负责 Wave B 三平台摘要。
- 现在完整 handoff 刷新入口已经统一收敛到 `scripts/prepare_wave_b_b2_handoff_bundle.sh`。
- cross summary 作为 handoff 链中的中间报告，不能再输出与 closure/consistency/handoff bundle 不一致的操作指导。

## Files
- `scripts/generate_wave_b_cross_platform_summary.sh`
- `tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，覆盖两个场景：
   - Linux `FAIL` 且 macOS/Windows 已 `PASS`
   - 三平台全部 `PASS`
2. 证明当前 cross summary `Next Actions` 仍是固定模板：
   - 不提示 Linux
   - 仍提示“重新运行本脚本”
   - 闭环后仍提示重复跑平台 lane
3. 最小修改 `generate_wave_b_cross_platform_summary.sh`：
   - 基于 Linux/macOS/Windows 当前状态动态生成 `Next Actions`
   - Linux 非 `PASS` 时显式提示修复 baseline
   - 统一改为指向 `prepare_wave_b_b2_handoff_bundle.sh`
4. 复跑 focused 与 handoff 邻近回归。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh
bash -n scripts/generate_wave_b_cross_platform_summary.sh
bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：cross summary `Next Actions` 不提 Linux、仍提示“重新运行本脚本”。
- 修复后：
  - Linux 非 `PASS` 时 `Next Actions` 明确出现 Linux
  - 三平台全 `PASS` 时改为已对齐/可选复核提示
  - 报告统一指向 `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- 邻近 handoff contracts 继续 PASS。
