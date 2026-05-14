# 2026-05-14 Wave B macOS Path-Check Live Passthrough

## Goal
收口 `run_wave_b_macos_gate.sh --path-check-live` 与 `run_macos_openssl_path_check_draft.sh` 之间的参数漂移，避免 path-check live 忽略 gate 的 `--openssl-root` / `--modules` / `--verbose` 真值。

## Architecture
- 当前 macOS gate live path-check 只调用：
  - `bash scripts/run_macos_openssl_path_check_draft.sh`
  - 只在 dry-run 时追加 `--dry-run`
- 但 path-check 子脚本自己的输入面是 CLI：
  - `--openssl-root DIR`
  - `--modules LIST`
  - `--verbose`
- 已坐实的真实漂移：
  - gate 提供自定义 `--openssl-root` 时，path-check live 会忽略它并直接失败
  - 即使 path-check live 成功，默认也会继续使用它自己的默认模块集，而不是 gate 的 `MODULE_SET`

## Files
- `scripts/run_wave_b_macos_gate.sh`
- `tests/scripts/test_wave_b_macos_gate_path_check_live_passthrough_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明 path-check live 目前没有透传 gate 的自定义 root / modules / verbose。
2. 最小修改 `run_wave_b_macos_gate.sh`，只给 path-check step 补参数透传。
3. 复跑新合同和 macOS gate 邻近合同。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_macos_gate_path_check_live_passthrough_contract.sh
bash tests/scripts/test_wave_b_macos_gate_path_check_live_passthrough_contract.sh
bash tests/scripts/test_wave_b_macos_gate_module_injection_contract.sh
bash tests/scripts/test_wave_b_macos_gate_openssl_root_injection_contract.sh
bash tests/scripts/test_wave_b_macos_gate_examples_threshold_contract.sh
bash -n scripts/run_wave_b_macos_gate.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - live path-check 忽略自定义 `openssl-root`
  - 可能直接报 `OpenSSL root not detected`
  - 即使继续执行，也会和 gate 主流程消费不同的模块集
- 修复后：
  - live path-check 与 gate 主流程消费同一组 `openssl-root` / `modules` / `verbose` 真值
  - summary 里的 path-check 能重新回到 PASS。
