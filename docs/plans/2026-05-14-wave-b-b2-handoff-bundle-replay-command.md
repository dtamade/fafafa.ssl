# 2026-05-14 Wave B/B2 Handoff Bundle Replay Command

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 在 handoff bundle 中生成的重跑命令缺口，避免报告只写 `--run-id --strict`，却丢掉当前批次实际使用的自定义 `linux summary/examples`、显式 macOS/Windows evidence、以及自定义 `output-dir`。

## Architecture
- handoff bundle 的 `Next Actions` 应提供一条可复现当前批次 truth 的命令，而不是只靠 run_id 猜默认路径
- replay command 至少应保留：
  - `--run-id`
  - `--linux-summary`
  - `--linux-examples`
  - `--output-dir`
  - 以及当前批次实际生效的 macOS/Windows top-level evidence args
- 取值原则：
  - Linux summary/examples 始终保留，避免“最新文件”漂移
  - macOS/Windows 只在显式传入或当前 active evidence 存在时保留
  - 默认 no-evidence 场景不应被错误升级成显式 missing

## Files
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_replay_command_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED contract，复现 custom summary/examples/windows-summary/output-dir 已参与本批次，但 bundle 的 replay command 仍只写 `--run-id --strict`。
2. 在 `prepare_wave_b_b2_handoff_bundle.sh` 中生成一条 shell-quoted replay command，保留当前批次关键 top-level args。
3. 跑 focused 合同、显式缺失 passthrough、Windows companion artifact list 邻近回归。
4. 更新 working memory，review 后提交。

## Commands
- `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_replay_command_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_replay_command_contract.sh`
- `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`
- `git diff --check`

## Expected Outputs
- handoff bundle 的 replay command 会保留当前批次关键自定义路径。
- 默认 no-evidence 场景不会被错误升级成显式 missing replay args。
- 现有 handoff artifact-list、explicit-missing passthrough、Windows companion 回归继续通过。
