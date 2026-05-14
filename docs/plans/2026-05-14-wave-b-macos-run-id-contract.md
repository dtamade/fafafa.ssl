# 2026-05-14 Wave B macOS Run ID Contract

## Goal
收口 `scripts/run_wave_b_macos_gate.sh` 的 `--run-id` 输入契约，避免非法字符打断 `bash -lc` 命令串，也避免空 `run-id` 继续落成空后缀产物。

## Architecture
- Linux 兄弟脚本 `run_wave_b_ci_gate.sh` 已经有更严格的 `run-id` 语义：
  - 非法字符直接拒绝
  - 空值回退到默认时间戳
- macOS 脚本当前仍保留两类真实问题：
  - `bad'quote` 这类值会直接破坏 step command 里的单引号拼接
  - `--run-id ""` 会生成 `wave_b_macos_*_.log/json/md` 和空 `run_id` summary

## Files
- `scripts/run_wave_b_macos_gate.sh`
- `tests/scripts/test_wave_b_macos_gate_invalid_run_id_contract.sh`
- `tests/scripts/test_wave_b_macos_gate_empty_run_id_fallback_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写两个 focused contracts，分别锁住 invalid run-id 的早拒绝语义和 empty run-id 的默认回退语义。
2. 最小修改脚本，让 `run-id` 校验/回退在任何 step 执行前完成。
3. 复跑两个新合同和现有 macOS gate focused contracts。
4. 更新 working-memory，review 后提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_macos_gate_invalid_run_id_contract.sh
bash tests/scripts/test_wave_b_macos_gate_invalid_run_id_contract.sh
bash -n tests/scripts/test_wave_b_macos_gate_empty_run_id_fallback_contract.sh
bash tests/scripts/test_wave_b_macos_gate_empty_run_id_fallback_contract.sh
bash tests/scripts/test_wave_b_macos_gate_output_dir_boundary_contract.sh
bash tests/scripts/test_wave_b_macos_gate_invalid_examples_threshold_contract.sh
bash tests/scripts/test_wave_b_macos_gate_examples_threshold_contract.sh
bash -n scripts/run_wave_b_macos_gate.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - 非法 `run-id` 不会被前置拒绝，step command 里的单引号拼接会被打断
  - 空 `run-id` 会生成空后缀文件和空的 summary `run_id`
- 修复后：
  - 非法 `run-id` 直接报 `Invalid --run-id`
  - 空 `run-id` 自动回退到默认时间戳
  - 任何产物都不再出现空后缀 `_`
