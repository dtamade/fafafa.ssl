# 2026-05-14 Wave B Examples JSON Parse Failure Truth

## Goal
统一 `run_wave_b_macos_gate.sh` 与 `run_wave_b_ci_gate.sh` 在 examples JSON 损坏时的真值，避免 macOS 硬中止、Linux 泄 traceback、两边 summary 行为继续漂移。

## Architecture
- 当前两条 gate 都依赖 `verify_examples_compile.sh` 产出的 JSON。
- 但一旦 helper `exit 0` 且 JSON 实际损坏：
  - macOS gate 会在 `json.load` 处直接异常中止，不产出 summary
  - Linux gate 会泄 Python traceback，但仍产出 FAIL summary
- 这批要把两边统一到同一 operator-facing truth：
  - examples step = `FAIL`
  - overall = `FAIL`
  - examples metrics = `n/a`
  - 不泄 traceback
  - summary 仍然落盘

## Files
- `scripts/run_wave_b_macos_gate.sh`
- `scripts/run_wave_b_ci_gate.sh`
- `tests/scripts/test_wave_b_macos_gate_invalid_examples_json_contract.sh`
- `tests/scripts/test_wave_b_ci_gate_invalid_examples_json_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 macOS / Linux focused contracts，证明当前坏 JSON 场景仍会异常中止或泄 traceback。
2. 最小修改两条 gate，只收口 examples JSON 解析失败路径。
3. 复跑新合同与现有 macOS / Linux gate focused contracts。
4. 更新 working-memory，review 后提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_macos_gate_invalid_examples_json_contract.sh
bash tests/scripts/test_wave_b_macos_gate_invalid_examples_json_contract.sh
bash -n tests/scripts/test_wave_b_ci_gate_invalid_examples_json_contract.sh
bash tests/scripts/test_wave_b_ci_gate_invalid_examples_json_contract.sh
bash tests/scripts/test_wave_b_macos_gate_module_injection_contract.sh
bash tests/scripts/test_wave_b_macos_gate_openssl_root_injection_contract.sh
bash tests/scripts/test_wave_b_macos_gate_invalid_run_id_contract.sh
bash tests/scripts/test_wave_b_macos_gate_empty_run_id_fallback_contract.sh
bash tests/scripts/test_wave_b_macos_gate_output_dir_boundary_contract.sh
bash tests/scripts/test_wave_b_macos_gate_invalid_examples_threshold_contract.sh
bash tests/scripts/test_wave_b_macos_gate_examples_threshold_contract.sh
bash tests/scripts/test_wave_b_ci_gate_examples_threshold_contract.sh
bash tests/scripts/test_wave_b_ci_gate_invalid_examples_threshold_contract.sh
bash -n scripts/run_wave_b_macos_gate.sh
bash -n scripts/run_wave_b_ci_gate.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - macOS：坏 JSON 会在 `json.load` 处异常中止，不产出 summary
  - Linux：坏 JSON 会泄 traceback，虽然仍落 FAIL summary
- 修复后：
  - macOS / Linux：都产出 FAIL summary
  - examples metrics 统一是 `n/a`
  - stderr 不再包含 `JSONDecodeError` / Python traceback
