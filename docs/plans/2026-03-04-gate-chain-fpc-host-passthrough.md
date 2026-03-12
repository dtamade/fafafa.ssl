# gate chain fpc host passthrough (2026-03-04)

## Goal
在 gate 链路脚本中贯通 `FAFAFA_FPC_EXE`，避免 gate 层仍隐式绑定 `fpc` 命令名。

## Scope
- `scripts/run_minimal_ci_gate.sh`
- `scripts/run_wave_b_ci_gate.sh`
- `scripts/run_wave_b_macos_gate.sh`
- `scripts/run_wave_c_b101_validation_playbook.sh`

最小改动原则：仅调整命令拼接与参数透传，不改流程控制。

## Files
- Modify: `scripts/run_minimal_ci_gate.sh`
- Modify: `scripts/run_wave_b_ci_gate.sh`
- Modify: `scripts/run_wave_b_macos_gate.sh`
- Modify: `scripts/run_wave_c_b101_validation_playbook.sh`
- Add: `tests/scripts/test_minimal_ci_gate_fpc_host_passthrough_contract.sh`
- Add: `tests/scripts/test_wave_b_ci_gate_fpc_host_passthrough_contract.sh`
- Add: `tests/scripts/test_wave_b_macos_gate_fpc_host_passthrough_contract.sh`
- Add: `tests/scripts/test_wave_c_b101_validation_playbook_fpc_host_passthrough_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## RED -> GREEN -> Regression
1. RED
   - 新增 4 条合同，验证 gate 命令中包含：
     - compile_all_modules 的 `--fpc-exe` 透传
     - run_all_module_tests 的 `FAFAFA_FPC_EXE` 透传
     - wave_c benchmark 编译命令使用覆盖后的 FPC 主机
   - 首次运行应失败。
2. GREEN
   - 在 4 条脚本新增 `FPC_EXE="${FAFAFA_FPC_EXE:-fpc}"`。
   - compile 命令追加 `--fpc-exe '$FPC_EXE'`。
   - module 命令追加 `FAFAFA_FPC_EXE='$FPC_EXE'`。
   - wave_c benchmark 编译命令改用 `$FPC_EXE`。
3. Regression
   - 4 条新合同。
   - 既有合同：
     - `test_minimal_ci_gate_compile_module_isolation_passthrough.sh`
     - `test_wave_b_ci_gate_isolation_passthrough_contract.sh`
     - `test_wave_b_macos_gate_isolation_passthrough_contract.sh`
     - `test_wave_c_b101_validation_playbook_isolation_passthrough_contract.sh`
   - 相关脚本 + 新合同 `bash -n`。

## Expected
- gate 层对 FPC 主机选择可配置，且与下游脚本行为一致。
- 不引入流程/门禁逻辑变化。
