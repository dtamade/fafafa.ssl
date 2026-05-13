# 2026-05-14 Wave B/B2 Handoff Bundle Next Actions

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 中 handoff bundle `Next Actions` 的静态误导问题，避免报告在 Windows 已完成、甚至整包已 `CLOSED` 时仍固定提示去跑 macOS/Windows live gate。

## Architecture
- `Next Actions` 应该基于当前 bundle truth 动态生成，而不是固定三行模板
- 信号来源：
  - `closure report` 的 `macos/windows` 平台状态
  - 当前 Windows companion runtime artifacts 是否缺失
  - `handoff_state`
- 生成规则：
  - `macos_state != PASS` 才提示 macOS runner
  - `windows_state != PASS` 才提示 Windows summary/live gate
  - `windows_state == PASS` 但 companion runtime artifacts 缺失时，提示补 Windows runtime artifacts
  - `handoff_state == CLOSED` 时，不再提示多余 live gate；只保留闭环完成/可选复核提示

## Files
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED contract，覆盖“Windows 已 PASS 但 macOS 未完成”和“整包 CLOSED”两种误导场景。
2. 在 `prepare_wave_b_b2_handoff_bundle.sh` 中基于 closure/platform/runtime truth 动态生成 `Next Actions`。
3. 跑 focused 合同、replay-command、artifact-list、explicit-missing 邻近回归。
4. 更新 working memory，review 后提交。

## Commands
- `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`
- `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_replay_command_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`
- `git diff --check`

## Expected Outputs
- `READY_FOR_RUNNER` 且 Windows 已 PASS 时，不再提示重复跑 Windows。
- `CLOSED` 时不再提示跑 macOS/Windows live gate。
- replay command、artifact list、explicit-missing passthrough 既有合同继续通过。
