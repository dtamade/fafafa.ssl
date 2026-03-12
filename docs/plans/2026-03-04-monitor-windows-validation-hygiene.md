# monitor/windows validation hygiene (2026-03-04)

## Goal
修复两类审查发现的稳定性问题：
1) 持续监控脚本每轮模块测试未显式透传隔离输出目录；
2) Windows `validate_all_modules.ps1` 使用硬编码项目根路径，移植性差。

## Architecture / Scope
- `scripts/continuous_test_monitor.sh`
  - 每轮运行生成 run 级隔离目录：
    - `unit_output_dir`
    - `bin_output_dir`
  - 调用 `run_all_module_tests.sh` 时透传：
    - `FAFAFA_FPC_UNIT_OUTPUT_DIR`
    - `FAFAFA_TEST_BIN_DIR`
  - 保持历史统计解析逻辑不变。
- `scripts/validate_all_modules.ps1`
  - 新增可选参数：`-ProjectRoot`
  - 默认由脚本路径推导项目根，去除硬编码绝对路径。

## Files
- Modify: `scripts/continuous_test_monitor.sh`
- Modify: `scripts/validate_all_modules.ps1`
- Add: `tests/scripts/test_continuous_test_monitor_isolation_passthrough_contract.sh`
- Add: `tests/scripts/test_validate_all_modules_dynamic_project_root_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：新增两条静态合同，先验证当前脚本不满足要求。
2. GREEN：最小改造两个脚本（仅补隔离透传与动态路径）。
3. Regression：
   - `bash tests/scripts/test_continuous_test_monitor_isolation_passthrough_contract.sh`
   - `bash tests/scripts/test_validate_all_modules_dynamic_project_root_contract.sh`
   - `bash tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh`
   - `bash -n scripts/continuous_test_monitor.sh`

## Expected Outputs
- 持续监控脚本具备 run 级隔离透传能力，支持并发场景更稳健。
- Windows 验证脚本不再依赖固定盘符路径，默认可在仓库内任意位置复用。
