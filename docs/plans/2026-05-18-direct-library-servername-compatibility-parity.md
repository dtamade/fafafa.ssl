# Direct-Library ServerName Compatibility Parity

## Goal

把 deprecated `TSSLConfig.ServerName` 在 direct-library path 上的兼容语义从 “OpenSSL 一家独有” 收成跨 backend 的统一 truth：

- client default-config path:
  - warning + ignore
- server default-config path:
  - reject

本批不动 factory path，不动 builder path，也不把 early-data / replay-store 混进来。

## Architecture

- 目标路径：
  - `ISSLLibrary.SetDefaultConfig(...)`
  - `ISSLLibrary.CreateContext(AType)`
- 已有真相源：
  - OpenSSL direct-library path 已经实现：
    - client warning + ignore
    - server reject
- 本批要把这条规则推广到：
  - `TFreePascalSSLLibrary`
  - `TWinSSLLibrary`
  - `TMbedTLSLibrary`
  - `TWolfSSLLibrary`

## TDD

### RED

1. 新增 FreePascal direct-library runtime test：
   - 默认配置带 `ServerName`
   - client path 必须 warning + ignore
   - server path 必须 reject
2. 新增跨 backend source contract：
   - 每个 backend library unit 都必须：
     - server reject
     - client warning + ignore

### GREEN

- 在 4 个 backend library units 中补齐和 OpenSSL 对齐的 warning/reject 逻辑

### REGRESSION

- 复用现有 compatibility allowlist contract
- 复用 direct-library default-config parity contract

## Files

- Add: `docs/plans/2026-05-18-direct-library-servername-compatibility-parity.md`
- Add: `tests/test_freepascal_library_default_config_server_name_clarification.pas`
- Add: `tests/scripts/test_direct_library_servername_compatibility_contract.sh`
- Update: `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
- Update: `src/fafafa.ssl.freepascal.lib.pas`
- Update: `src/fafafa.ssl.winssl.lib.pas`
- Update: `src/fafafa.ssl.mbedtls.lib.pas`
- Update: `src/fafafa.ssl.wolfssl.lib.pas`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`

## Commands

1. `bash -n tests/scripts/test_direct_library_servername_compatibility_contract.sh`
2. `bash tests/scripts/test_direct_library_servername_compatibility_contract.sh`
3. `mkdir -p tmp/test_freepascal_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_library_default_config_server_name_clarification -FEtmp/test_freepascal_library_default_config_server_name_clarification -otmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification tests/test_freepascal_library_default_config_server_name_clarification.pas && ./tmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification`
4. `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
5. `bash tests/scripts/test_direct_library_default_config_parity_contract.sh`
6. `git diff --check`

## Expected Outputs

- RED 时：
  - FreePascal direct-library runtime test 失败
  - source contract 失败
- GREEN 后：
  - FreePascal direct-library runtime test 通过
  - source contract 证明 5 个 backend direct-library path 的 `ServerName` compatibility 语义已对齐
  - allowlist / default-config parity contracts 继续为绿
