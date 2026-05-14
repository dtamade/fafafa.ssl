# 2026-05-14 Wave B macOS Invalid Examples Threshold

## Goal
补齐 `scripts/run_wave_b_macos_gate.sh` 对非法 `--examples-threshold` 输入的前置校验，避免脚本在门禁阶段才因 `float(...)` 解析失败而误分类成普通 gate FAIL。

## Architecture
- Linux 兄弟脚本 `run_wave_b_ci_gate.sh` 已有一致的输入校验契约：
  - 非法阈值会立即报错
  - 不执行任何 gate step
  - 不生成 summary
- macOS 脚本当前缺少同样的前置校验：
  - `EXAMPLES_THRESHOLD` 直接进入后续 `python3 float(...)`
  - 这意味着错误会在步骤执行后才暴露
  - 用户拿到的是误导性的 gate 失败，而不是明确的参数错误

## Files
- `scripts/run_wave_b_macos_gate.sh`
- `tests/scripts/test_wave_b_macos_gate_invalid_examples_threshold_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明非法阈值当前不会被前置拒绝。
2. 最小补齐参数校验逻辑。
3. 复跑 focused 合同、脚本语法检查和 diff hygiene。
4. 更新 working-memory，review 后提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_macos_gate_invalid_examples_threshold_contract.sh
bash tests/scripts/test_wave_b_macos_gate_invalid_examples_threshold_contract.sh
bash -n scripts/run_wave_b_macos_gate.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - 非法阈值没有被前置拦截
  - gate steps 已开始执行，或 summary 已被写出
  - 报错点落在后续阈值比较逻辑
- 修复后：
  - 脚本直接报 `Invalid --examples-threshold`
  - 不执行任何 gate step
  - 不生成 summary
