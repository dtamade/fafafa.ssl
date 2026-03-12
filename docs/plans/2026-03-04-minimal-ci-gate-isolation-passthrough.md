# minimal_ci_gate isolation passthrough (2026-03-04)

## Goal
将编译/模块测试的隔离输出策略透传到 `scripts/run_minimal_ci_gate.sh`，降低上层 gate 并发调度时的产物冲突概率。

## Architecture / Scope
- 在 minimal gate 中增加 run 级隔离变量：
  - `RUN_ID`
  - `COMPILE_UNIT_OUTPUT_DIR`
  - `MODULE_UNIT_OUTPUT_DIR`
  - `MODULE_BIN_OUTPUT_DIR`
- compile 步骤透传到 `compile_all_modules.py`：
  - `--unit-output-dir`
- modules 步骤透传到 `run_all_module_tests.sh`：
  - `FAFAFA_FPC_UNIT_OUTPUT_DIR`
  - `FAFAFA_TEST_BIN_DIR`
- 保持 existing only-platform/docs-governance dry-run 契约行为不变。

## Files
- Modify: `scripts/run_minimal_ci_gate.sh`
- Add: `tests/scripts/test_minimal_ci_gate_compile_module_isolation_passthrough.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：新增 dry-run 合同，要求 compile/module 命令包含隔离参数透传。
2. RED 验证：运行合同，预期失败（尚未透传）。
3. GREEN：最小改造 minimal gate 命令构造逻辑，接入隔离变量。
4. Regression：
   - `bash tests/scripts/test_minimal_ci_gate_compile_module_isolation_passthrough.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh`
   - `bash tests/scripts/test_minimal_ci_gate_docs_governance_batch_option.sh`
   - `bash -n scripts/run_minimal_ci_gate.sh`

## Expected Outputs
- 新合同通过，compile/module 命令可见隔离参数透传。
- 既有 minimal gate dry-run 契约全部保持通过。
