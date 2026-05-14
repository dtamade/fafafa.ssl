# 2026-05-14 Wave B macOS Examples Threshold Truth

## Goal
收口 `scripts/run_wave_b_macos_gate.sh` 对 examples gate 的判定语义漂移，避免 macOS gate 在 `pass_rate` 已达阈值时，仅因为 helper 报告了少量 failed files 就继续误判 FAIL。

## Architecture
- `run_wave_b_macos_gate.sh` 当前和 Linux 旧逻辑同型：
  - 先读取 `examples_compile_gate_macos_<run_id>.json`
  - 再计算 `pass_rate >= threshold`
  - 但最终仍要求 `examples_exit == 0 && threshold_pass == true`
- 这会制造和 Linux 之前同型的假失败：
  - helper 退出码表达“仍有 failed files”
  - 上层 gate 契约却写的是阈值门禁
  - 两层真值被混在一起

## Files
- `scripts/run_wave_b_macos_gate.sh`
- `tests/scripts/test_wave_b_macos_gate_examples_threshold_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，用 fake macOS gate 工程证明 `pass_rate` 达阈值时，当前 gate 仍因 helper `exit 1` 落成 FAIL。
2. 最小修改 `run_wave_b_macos_gate.sh`，让 examples step 真值只收口到阈值判定。
3. 复跑 focused 合同与脚本语法检查。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_macos_gate_examples_threshold_contract.sh
bash tests/scripts/test_wave_b_macos_gate_examples_threshold_contract.sh
bash -n scripts/run_wave_b_macos_gate.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - helper `exit 1`
  - `pass_rate=94.7 >= 80.0`
  - macOS gate 仍把 examples step / overall 判成 FAIL
- 修复后：
  - examples step 按阈值判成 PASS
  - overall 也保持 PASS
  - summary 继续保留 helper `exit 1` 作为 evidence。
