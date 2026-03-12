# linux/macos path-check isolation passthrough (2026-03-04)

## Goal
将 compile/module 隔离输出策略继续上推到 Linux matrix 与 macOS path-check 脚本，确保上层路径检查入口在并发场景下也能复用隔离能力。

## Architecture / Scope
- Linux matrix (`run_linux_openssl_matrix_draft.sh`)
  - 新增 run 级隔离变量：
    - `RUN_ID`
    - `COMPILE_UNIT_OUTPUT_DIR`
    - `MODULE_UNIT_OUTPUT_DIR`
    - `MODULE_BIN_OUTPUT_DIR`
  - compile 步骤透传 `--unit-output-dir`
  - modules 步骤透传 `FAFAFA_FPC_UNIT_OUTPUT_DIR` 与 `FAFAFA_TEST_BIN_DIR`
- macOS path-check (`run_macos_openssl_path_check_draft.sh`)
  - 新增 run 级隔离变量：
    - `RUN_ID`
    - `MODULE_UNIT_OUTPUT_DIR`
    - `MODULE_BIN_OUTPUT_DIR`
  - modules 步骤透传 `FAFAFA_FPC_UNIT_OUTPUT_DIR` 与 `FAFAFA_TEST_BIN_DIR`
- 保持既有 dry-run 契约语义不变（PASS 行与 root 路径命令可见性）。

## Files
- Modify: `scripts/run_linux_openssl_matrix_draft.sh`
- Modify: `scripts/run_macos_openssl_path_check_draft.sh`
- Add: `tests/scripts/test_linux_openssl_matrix_isolation_passthrough_contract.sh`
- Add: `tests/scripts/test_macos_openssl_path_check_isolation_passthrough_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：新增两条 dry-run 合同，要求 Linux/macos 脚本命令输出出现隔离透传参数。
2. RED 验证：运行两条合同，预期失败。
3. GREEN：最小改造两个脚本并接入透传。
4. Regression：
   - `bash tests/scripts/test_linux_openssl_matrix_isolation_passthrough_contract.sh`
   - `bash tests/scripts/test_macos_openssl_path_check_isolation_passthrough_contract.sh`
   - `bash tests/scripts/test_linux_macos_matrix_draft_dryrun_contract.sh`
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash tests/scripts/test_linux_openssl_matrix_report_output_contract.sh`
   - `bash tests/scripts/test_android_openssl_path_check_draft_dryrun_contract.sh`
   - `bash -n scripts/run_linux_openssl_matrix_draft.sh`
   - `bash -n scripts/run_macos_openssl_path_check_draft.sh`

## Expected Outputs
- 新增 Linux/macos isolation passthrough 合同全部通过。
- 既有 Linux/macos dry-run 契约不回归。
- 相关多平台 dry-run batch 契约保持通过。
