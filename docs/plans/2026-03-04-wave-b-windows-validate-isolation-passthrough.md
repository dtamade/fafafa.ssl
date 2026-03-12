# wave_b windows validate isolation passthrough (2026-03-04)

## Goal
补齐 Windows gate 链路的两处工程化缺口：
1) `run_wave_b_windows_gate.ps1` 对 `validate_all_modules.ps1` 的显式参数透传；
2) `validate_all_modules.ps1` 在 FPC 编译时使用隔离 `-FU` 输出目录。

## Architecture / Scope
- `scripts/run_wave_b_windows_gate.ps1`
  - modules 步骤改为显式透传：
    - `-ProjectRoot`
    - `-UnitOutputDir`
- `scripts/validate_all_modules.ps1`
  - 新增参数：`-UnitOutputDir`
  - 默认按 `projectRoot/tmp/...` 生成 run 级隔离目录
  - 编译命令增加 `-FU<resolvedUnitOutputDir>`

## Files
- Modify: `scripts/run_wave_b_windows_gate.ps1`
- Modify: `scripts/validate_all_modules.ps1`
- Add: `tests/scripts/test_wave_b_windows_gate_validate_modules_passthrough_contract.sh`
- Add: `tests/scripts/test_validate_all_modules_unit_output_isolation_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：新增两条静态合同，预期当前实现不满足。
2. GREEN：最小改造两条 PowerShell 脚本，补参数透传和 `-FU` 隔离。
3. Regression：
   - `bash tests/scripts/test_wave_b_windows_gate_validate_modules_passthrough_contract.sh`
   - `bash tests/scripts/test_validate_all_modules_unit_output_isolation_contract.sh`
   - `bash tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh`
   - `bash -n tests/scripts/test_wave_b_windows_gate_validate_modules_passthrough_contract.sh tests/scripts/test_validate_all_modules_unit_output_isolation_contract.sh`

## Expected Outputs
- Windows gate modules 步骤包含 `-ProjectRoot` 与 `-UnitOutputDir`。
- validate 脚本支持并默认启用隔离单元输出目录。
- 既有 blocker batch 集成合同不回归。
