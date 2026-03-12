# android/windows path-check isolation passthrough (2026-03-04)

## Goal
将模块测试隔离输出策略继续上推到 Android 与 Windows path-check 草案脚本，确保这两个跨平台入口在并发场景下不会共享 `ppu/o` 与测试二进制输出目录。

## Architecture / Scope
- Android path-check (`run_android_openssl_path_check_draft.sh`)
  - 新增 run 级隔离变量：
    - `RUN_ID`
    - `MODULE_UNIT_OUTPUT_DIR`
    - `MODULE_BIN_OUTPUT_DIR`
  - module 步骤透传：
    - `FAFAFA_FPC_UNIT_OUTPUT_DIR`
    - `FAFAFA_TEST_BIN_DIR`
- Windows path-check (`run_windows_winssl_path_check_draft.sh`)
  - 新增 run 级隔离变量：
    - `RUN_ID`
    - `MODULE_UNIT_OUTPUT_DIR`
    - `MODULE_BIN_OUTPUT_DIR`
  - module 步骤透传：
    - `FAFAFA_FPC_UNIT_OUTPUT_DIR`
    - `FAFAFA_TEST_BIN_DIR`
- 保持既有 dry-run 合同语义不变（PASS 行、非目标平台 warning、项目根路径命令拼装）。

## Files
- Modify: `scripts/run_android_openssl_path_check_draft.sh`
- Modify: `scripts/run_windows_winssl_path_check_draft.sh`
- Add: `tests/scripts/test_android_openssl_path_check_isolation_passthrough_contract.sh`
- Add: `tests/scripts/test_windows_winssl_path_check_isolation_passthrough_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：新增 Android/Windows isolation passthrough 合同，要求 dry-run 输出出现
   - `FAFAFA_FPC_UNIT_OUTPUT_DIR='...'`
   - `FAFAFA_TEST_BIN_DIR='...'`
2. RED 验证：
   - `bash tests/scripts/test_android_openssl_path_check_isolation_passthrough_contract.sh`
   - `bash tests/scripts/test_windows_winssl_path_check_isolation_passthrough_contract.sh`
3. GREEN：最小改造两条 path-check 脚本，仅在 module 步骤增加 run 级隔离变量与 env 透传。
4. Regression：
   - `bash tests/scripts/test_android_openssl_path_check_isolation_passthrough_contract.sh`
   - `bash tests/scripts/test_windows_winssl_path_check_isolation_passthrough_contract.sh`
   - `bash tests/scripts/test_android_openssl_path_check_draft_dryrun_contract.sh`
   - `bash tests/scripts/test_windows_winssl_path_check_draft_dryrun_contract.sh`
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash tests/scripts/test_android_openssl_path_check_draft_failure_contract.sh`
   - `bash tests/scripts/test_windows_winssl_path_check_draft_failure_contract.sh`
   - `bash -n scripts/run_android_openssl_path_check_draft.sh`
   - `bash -n scripts/run_windows_winssl_path_check_draft.sh`

## Expected Outputs
- Android/Windows 新增 isolation passthrough 合同在 GREEN 后通过。
- 既有 Android/Windows dry-run 合同保持通过。
- Linux multi-platform dry-run batch 合同保持通过。
- Android/Windows failure 合同保持通过。
