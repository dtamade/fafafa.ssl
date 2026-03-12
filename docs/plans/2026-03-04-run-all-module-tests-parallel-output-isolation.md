# run_all_module_tests parallel output isolation (2026-03-04)

## Goal
进一步提升 `scripts/run_all_module_tests.sh` 并发稳定性，消除并行执行时共享输出路径导致的产物覆盖风险。

## Architecture / Scope
- 为单次运行引入 `RUN_ID`，用于报告与日志文件名隔离。
- 保持默认 `bin/` 行为不变，同时增加可覆盖的二进制输出目录：
  - `FAFAFA_TEST_BIN_DIR`
- 将报告与中间日志文件名改为带 `RUN_ID` 后缀：
  - `test_report_<RUN_ID>.txt`
  - `<test>_compile_<RUN_ID>.log`
  - `<test>_result_<RUN_ID>.txt`
- 新增并行合同测试，验证两个并发实例在隔离输出目录下可同时成功，并且报告文件路径不冲突。

## Files
- Modify: `scripts/run_all_module_tests.sh`
- Add: `tests/scripts/test_run_all_module_tests_parallel_output_isolation_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：新增并行合同，要求双进程并发运行均成功且报告路径不同。
2. GREEN：最小改造 `run_all_module_tests.sh`，接入 `RUN_ID` 文件名隔离和 `FAFAFA_TEST_BIN_DIR`。
3. Regression：
   - `bash tests/scripts/test_run_all_module_tests_unit_output_isolation_contract.sh`
   - `bash tests/scripts/test_run_all_module_tests_parallel_output_isolation_contract.sh`
   - `bash scripts/run_all_module_tests.sh --modules PKCS7 --verbose`
   - `bash tests/scripts/test_compile_all_modules_unit_output_isolation_contract.sh`

## Expected Outputs
- 并行合同通过，验证两路并发运行不共享报告文件。
- 单路样本运行保持通过，日志输出可见 `Run ID` 与 `Binary output dir`。
