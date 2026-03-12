# run_all_module_tests fpc relative-host resolution (2026-03-04)

## Goal
增强 `scripts/run_all_module_tests.sh` 对 `FAFAFA_FPC_EXE` 相对路径输入的兼容性，使其在非仓库工作目录下仍能稳定解析。

## Architecture / Scope
- `scripts/run_all_module_tests.sh`
  - 当 `FAFAFA_FPC_EXE` 含路径分隔符且为相对路径时，按 `PROJECT_ROOT` 解析为绝对路径。
  - 保持已存在的命令名与绝对路径校验逻辑。
- 新增运行态合同
  - 从 `/tmp` 调用脚本，传入相对 `FAFAFA_FPC_EXE` 包装器路径，验证脚本可正常运行并实际调用该包装器。

## Files
- Modify: `scripts/run_all_module_tests.sh`
- Add: `tests/scripts/test_run_all_module_tests_fpc_host_relative_runtime_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：
   - 新增运行态合同，验证 `/tmp` 启动 + 相对 `FAFAFA_FPC_EXE` 路径场景。
2. GREEN：
   - 最小改造脚本，相对路径统一按 `PROJECT_ROOT` 解析。
3. Regression：
   - 新运行态合同。
   - 既有 FPC host 合同与隔离合同：
     - `test_run_all_module_tests_fpc_host_override_contract.sh`
     - `test_run_all_module_tests_unit_output_isolation_contract.sh`
     - `test_run_all_module_tests_parallel_output_isolation_contract.sh`
   - `bash -n` 语法检查。

## Expected Outputs
- 在任意 cwd 下使用相对 `FAFAFA_FPC_EXE` 时不再误解析。
- 既有模块测试入口行为保持兼容。
