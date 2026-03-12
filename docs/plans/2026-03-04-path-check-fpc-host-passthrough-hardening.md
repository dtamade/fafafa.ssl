# multi-platform path-check fpc host passthrough hardening (2026-03-04)

## Goal
为三条跨平台 path-check 入口脚本补齐 `FAFAFA_FPC_EXE` 主机覆盖能力，消除 `fpc` 固定命令名假设，并将主机设置透传到模块测试步骤。

## Architecture / Scope
- `scripts/run_macos_openssl_path_check_draft.sh`
- `scripts/run_android_openssl_path_check_draft.sh`
- `scripts/run_windows_winssl_path_check_draft.sh`

改造目标：
- 引入 `FPC_EXE="${FAFAFA_FPC_EXE:-fpc}"`。
- 在 live 模式做可执行校验；dry-run 模式保留命令预览并告警。
- `run_cmd "... -iV"` 改为使用解析后的 `FPC_EXE`。
- module 步骤透传 `FAFAFA_FPC_EXE='$FPC_EXE'` 给 `scripts/run_all_module_tests.sh`。

## Files
- Modify: `scripts/run_macos_openssl_path_check_draft.sh`
- Modify: `scripts/run_android_openssl_path_check_draft.sh`
- Modify: `scripts/run_windows_winssl_path_check_draft.sh`
- Add: `tests/scripts/test_macos_openssl_path_check_fpc_host_passthrough_contract.sh`
- Add: `tests/scripts/test_android_openssl_path_check_fpc_host_passthrough_contract.sh`
- Add: `tests/scripts/test_windows_winssl_path_check_fpc_host_passthrough_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - 新增三条 dry-run 合同，断言：
     - 预检命令使用覆盖后的 `FAFAFA_FPC_EXE`（例如 `contract-fpc -iV`）
     - module 命令显式透传 `FAFAFA_FPC_EXE='contract-fpc'`
   - 初次执行应失败（脚本仍固定 `fpc` 且未透传）。
2. GREEN:
   - 对三脚本做最小改造，引入 `FPC_EXE` 解析/校验与 module 透传。
3. Regression:
   - 三条新合同。
   - 既有合同：
     - `test_macos_openssl_path_check_isolation_passthrough_contract.sh`
     - `test_android_openssl_path_check_isolation_passthrough_contract.sh`
     - `test_windows_winssl_path_check_isolation_passthrough_contract.sh`
     - `test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash -n` 语法检查（3 脚本 + 3 新合同）。

## Expected Outputs
- path-check 入口不再绑定 `fpc` 固定命令名。
- 干跑与实跑场景均可通过 `FAFAFA_FPC_EXE` 统一控制编译器主机。
- 既有跨平台 dry-run 合同行为保持兼容。
