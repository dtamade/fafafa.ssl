# run_all_module_tests unit-output isolation (2026-03-04)

## Goal
为 `scripts/run_all_module_tests.sh` 的 `fpc` 编译流程增加隔离的 unit 输出目录（`-FU`），降低并发执行测试脚本时的产物目录竞争风险。

## Architecture / Scope
- 在脚本内引入 `FPC_UNIT_OUTPUT_DIR` 变量并注入 `fpc` 命令参数 `-FU"$FPC_UNIT_OUTPUT_DIR"`。
- 目录策略：
  - 默认自动创建 `tmp/run_all_module_tests_units_<timestamp>_<pid>`。
  - 支持通过环境变量显式覆盖以便排障。
- 保持现有 CLI 参数与模块执行语义不变。
- 新增脚本合同测试，确保隔离参数不会被后续修改回退。

## Files
- Modify: `scripts/run_all_module_tests.sh`
- Add: `tests/scripts/test_run_all_module_tests_unit_output_isolation_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：新增合同测试，断言脚本中存在 `-FU"$FPC_UNIT_OUTPUT_DIR"` 并设置了默认目录策略。
2. RED 验证：运行合同测试，预期失败。
3. GREEN：最小改造 `run_all_module_tests.sh`，接入隔离输出目录并保留原有行为。
4. Regression：
   - `bash tests/scripts/test_run_all_module_tests_unit_output_isolation_contract.sh`
   - `bash scripts/run_all_module_tests.sh --modules PKCS7 --verbose`

## Expected Outputs
- 新合同测试通过，确认脚本层面的隔离编译参数存在。
- `run_all_module_tests.sh` 在至少一个模块样本上可正常完成，且不影响现有报告逻辑。
