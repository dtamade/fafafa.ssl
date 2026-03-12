# run_all_module_tests fpc host override (2026-03-04)

## Goal
为 `scripts/run_all_module_tests.sh` 增加可覆盖的 FPC 宿主解析能力，降低对默认 `fpc` 命令名的耦合。

## Architecture / Scope
- `scripts/run_all_module_tests.sh`
  - 新增 `FAFAFA_FPC_EXE` 覆盖入口（默认 `fpc`）。
  - 支持绝对路径/命令名两类可执行解析。
  - 编译调用改为使用解析后的 `$FPC_EXE`。

## Files
- Modify: `scripts/run_all_module_tests.sh`
- Add: `tests/scripts/test_run_all_module_tests_fpc_host_override_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：
   - 新增静态合同，锁定 `FAFAFA_FPC_EXE` 解析与变量化调用。
2. GREEN：
   - 最小改造脚本并保持模块测试流程不变。
3. Regression：
   - 新合同。
   - 既有 run_all_module_tests 合同：
     - `test_run_all_module_tests_unit_output_isolation_contract.sh`
     - `test_run_all_module_tests_parallel_output_isolation_contract.sh`
   - `bash -n` 语法检查。

## Expected Outputs
- 脚本可通过 `FAFAFA_FPC_EXE` 显式指定编译器。
- 默认行为保持兼容，已有并行隔离合同不回归。
