# 2026-05-14 Wave B macOS Output Dir Boundary

## Goal
补齐 `scripts/run_wave_b_macos_gate.sh` 对 `--output-dir` 的路径边界校验，避免 gate 产物被写出 fake 项目根之外。

## Architecture
- Linux 兄弟脚本已经把报告目录收敛到项目根内：
  - 绝对路径直接拒绝
  - `..` 越界路径也会拒绝
- macOS 脚本当前仍直接拼接：
  - `OUTPUT_DIR="$PROJECT_ROOT/$OUTPUT_DIR_REL"`
  - 后续命令也直接消费 `$OUTPUT_DIR_REL`
- 这让 `--output-dir ../../...` 成为真实越界写入口：
  - probe/log/examples json/summary 都能落到 fake 项目根之外
  - gate 甚至可能整体 exit 0

## Files
- `scripts/run_wave_b_macos_gate.sh`
- `tests/scripts/test_wave_b_macos_gate_output_dir_boundary_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明 `../../...` 输出目录当前会越界写出。
2. 最小补齐 `output-dir` 的 relative + stay-within-project-root 校验。
3. 复跑 focused 合同、既有 macOS gate 合同、脚本语法检查和 diff hygiene。
4. 更新 working-memory，review 后提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_macos_gate_output_dir_boundary_contract.sh
bash tests/scripts/test_wave_b_macos_gate_output_dir_boundary_contract.sh
bash tests/scripts/test_wave_b_macos_gate_invalid_examples_threshold_contract.sh
bash tests/scripts/test_wave_b_macos_gate_examples_threshold_contract.sh
bash -n scripts/run_wave_b_macos_gate.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - `--output-dir ../../...` 被接受
  - 产物写到 fake 项目根之外
  - gate 可能仍 exit 0
- 修复后：
  - 脚本直接报 `Invalid --output-dir`
  - 不执行任何 gate step
  - 越界目录没有新产物
