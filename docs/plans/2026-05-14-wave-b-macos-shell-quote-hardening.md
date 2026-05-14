# 2026-05-14 Wave B macOS Shell Quote Hardening

## Goal
收口 `scripts/run_wave_b_macos_gate.sh` 对 `--modules` 与 `--openssl-root` 的 shell 拼接风险，避免调用方输入被当成 `bash -lc` 代码执行。

## Architecture
- 当前 macOS gate 仍直接把动态输入拼进 step command 字符串：
  - `MODULE_SET` 直接插入 `modules_cmd`
  - `OPENSSL_ROOT` 直接插入 `ENV_PREFIX`
- 这已经被实锤成两个真实风险：
  - `--modules "PKCS7; touch '$MARKER'; #"` 会执行注入 payload，且 gate 仍可整体 `exit 0`
  - `--openssl-root "/tmp/ssl'; touch '$MARKER'; echo '"` 会在每个 step 前执行注入 payload
- 仓库里已有 `printf '%q'` 先例，可直接作为本批最小 shell-safe 拼装策略。

## Files
- `scripts/run_wave_b_macos_gate.sh`
- `tests/scripts/test_wave_b_macos_gate_module_injection_contract.sh`
- `tests/scripts/test_wave_b_macos_gate_openssl_root_injection_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contracts，分别锁住 `modules` 与 `openssl-root` 的注入风险。
2. 最小修改脚本，改成 shell-safe 参数拼装。
3. 复跑新合同和现有 macOS gate focused contracts。
4. 更新 working-memory，review 后提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_macos_gate_module_injection_contract.sh
bash tests/scripts/test_wave_b_macos_gate_module_injection_contract.sh
bash -n tests/scripts/test_wave_b_macos_gate_openssl_root_injection_contract.sh
bash tests/scripts/test_wave_b_macos_gate_openssl_root_injection_contract.sh
bash tests/scripts/test_wave_b_macos_gate_invalid_run_id_contract.sh
bash tests/scripts/test_wave_b_macos_gate_empty_run_id_fallback_contract.sh
bash tests/scripts/test_wave_b_macos_gate_output_dir_boundary_contract.sh
bash tests/scripts/test_wave_b_macos_gate_invalid_examples_threshold_contract.sh
bash tests/scripts/test_wave_b_macos_gate_examples_threshold_contract.sh
bash -n scripts/run_wave_b_macos_gate.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - `modules` payload 会作为 shell 语法执行
  - `openssl-root` payload 会作为环境前缀 shell 语法执行
- 修复后：
  - 两条输入都只作为数据透传
  - marker 不再被创建
  - fake nested runner / fake probe 仍能观察到完整原始值
