# wave_b/wave_c gate isolation passthrough (2026-03-04)

## Goal
将 compile/module 隔离输出策略上推到 Wave B/Wave C 的高层 gate 脚本，避免并发执行时回落到共享输出目录。

## Architecture / Scope
- `scripts/run_wave_b_ci_gate.sh`
  - 新增 run 级隔离变量：
    - `RUN_ID`（支持 env 覆盖）
    - `COMPILE_UNIT_OUTPUT_DIR`
    - `MODULE_UNIT_OUTPUT_DIR`
    - `MODULE_BIN_OUTPUT_DIR`
  - compile 步骤透传：`--unit-output-dir`
  - module 步骤透传：`FAFAFA_FPC_UNIT_OUTPUT_DIR` + `FAFAFA_TEST_BIN_DIR`
- `scripts/run_wave_b_macos_gate.sh`
  - 新增 run 级隔离变量：
    - `COMPILE_UNIT_OUTPUT_DIR`
    - `MODULE_UNIT_OUTPUT_DIR`
    - `MODULE_BIN_OUTPUT_DIR`
  - compile 步骤透传：`--unit-output-dir`
  - module 步骤透传：`FAFAFA_FPC_UNIT_OUTPUT_DIR` + `FAFAFA_TEST_BIN_DIR`
- `scripts/run_wave_c_b101_validation_playbook.sh`
  - 新增 run 级隔离变量：
    - `COMPILE_UNIT_OUTPUT_DIR`
    - `MODULE_UNIT_OUTPUT_DIR`
    - `MODULE_BIN_OUTPUT_DIR`
  - full-gate compile/module 透传同上
- 保持既有 dry-run 与 summary 语义不变。

## Files
- Modify: `scripts/run_wave_b_ci_gate.sh`
- Modify: `scripts/run_wave_b_macos_gate.sh`
- Modify: `scripts/run_wave_c_b101_validation_playbook.sh`
- Add: `tests/scripts/test_wave_b_ci_gate_isolation_passthrough_contract.sh`
- Add: `tests/scripts/test_wave_b_macos_gate_isolation_passthrough_contract.sh`
- Add: `tests/scripts/test_wave_c_b101_validation_playbook_isolation_passthrough_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：新增三条 dry-run 合同，检查 compile/module 命令中的隔离参数透传。
2. RED 验证：
   - `bash tests/scripts/test_wave_b_ci_gate_isolation_passthrough_contract.sh`
   - `bash tests/scripts/test_wave_b_macos_gate_isolation_passthrough_contract.sh`
   - `bash tests/scripts/test_wave_c_b101_validation_playbook_isolation_passthrough_contract.sh`
3. GREEN：最小改造 3 条脚本，仅增加 run 级隔离变量和 compile/module 命令透传。
4. Regression：
   - 三条新合同
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash -n scripts/run_wave_b_ci_gate.sh`
   - `bash -n scripts/run_wave_b_macos_gate.sh`
   - `bash -n scripts/run_wave_c_b101_validation_playbook.sh`

## Expected Outputs
- 三条新增 isolation passthrough 合同通过。
- 三个 gate 脚本 dry-run 模式仍可稳定输出命令与 summary。
- 既有跨平台 path-check dry-run batch 合同不回归。
