# Wave B / B2: Windows modules gate 修复（OpenSSL 单元清单漂移 → 动态扫描 + 阈值门禁）

## Goal
- 修复 Windows Wave B gate 的 `modules` 步骤“假阳性 PASS”风险：确保 `scripts/validate_all_modules.ps1` 编译的确是仓库当前真实存在的 OpenSSL 单元集合，并在扫描到的模块数异常偏少时直接 FAIL。

## Background / Problem
- `scripts/validate_all_modules.ps1` 之前硬编码了 `fafafa.ssl.openssl.core.pas` 等旧路径。
- 但当前仓库 OpenSSL API 单元已迁移为 `fafafa.ssl.openssl.api.*.pas`，导致脚本大量 `Test-Path` miss，只记 warning、不记失败，最终可能出现：
  - “只编译到极少数模块，但 exit=0”的假阳性；
  - Windows Wave B `modules` 步骤失去门禁价值。

## Fix
1) 改为按文件系统动态扫描 OpenSSL 单元：
   - `src/fafafa.ssl.openssl*.pas`
2) 增加最小数量阈值（默认 `MinModuleCount=50`）：
   - 扫描到的 OpenSSL 单元数小于阈值则直接 `exit 1`，防止脚本在路径/环境异常时误 PASS。
3) 编译输出隔离：
   - 将 FPC 单元输出写入 `test-reports/validate_all_modules_units_<run_id>/`，避免污染 `src/`。
4) 增加脚本契约测试，防止回归：
   - 检查 `Get-ChildItem` 扫描逻辑与阈值门禁存在；
   - 禁止旧模块路径 `fafafa.ssl.openssl.core.pas` 回流；
   - 禁止自定义 `[switch]$Verbose`（避免 common `-Verbose` 冲突）。

## Files
- Modify: `scripts/validate_all_modules.ps1`
- Add: `tests/scripts/test_validate_all_modules_module_scan_and_threshold_contract.sh`

## Verification
1) 运行脚本契约测试（Linux/macOS 可跑）
   - `bash tests/scripts/test_validate_all_modules_module_scan_and_threshold_contract.sh`
2) （CI/Runner）触发 `.github/workflows/wave-b-b2-manual.yml`
   - 预期：Windows gate 的 `modules` 步骤能真实覆盖 OpenSSL 单元，且在模块数异常时 fail-fast

## Expected Outputs / Acceptance
- `test_validate_all_modules_module_scan_and_threshold_contract` => PASS
- Windows gate 产物中 `wave_b_windows_modules_<run_id>.log` 不再充斥“文件不存在” warning
- Windows gate 产物中 `validate_all_modules_report_<run_id>.md` 存在且可直接审查失败模块与统计
- 若 runner 环境或脚本路径异常导致模块扫描不足，`modules` 步骤必须 `FAIL`（exit!=0）
