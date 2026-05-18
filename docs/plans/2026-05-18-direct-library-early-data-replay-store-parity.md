# Direct-Library Early-Data Replay-Store Parity

## Goal

把 `ISSLLibrary.SetDefaultConfig(...)` + `ISSLLibrary.CreateContext(AType)` 这条 direct-library path 上最后一组未对齐的特殊语义收口到和 factory/context path 同一套 truth：

- `ClientEarlyDataEnabled`
- `ServerEarlyDataPolicy`
- `ServerMaxEarlyDataSize`
- `ServerEarlyDataReplayStoreFile`
- `ServerEarlyDataReplayStoreDirectory`

本批不扩 capability，不碰 `ISSLConnection` 大手术，也不回退 `ServerName` 已冻结的 compatibility truth。

## Architecture

- 直接对齐的真相源：
  - `TSSLFactory.CreateContext(AContextType, ALibType)`
  - `TSSLFactory.CreateContext(const AConfig)`
- 预期 direct-library 行为：
  - 支持 `ISSLEarlyDataContext` 的 backend，要把 early-data 默认字段套到新 context
  - replay-store 两个字段保持 server-only 约束
  - replay-store file / directory 保持 mutually exclusive
  - 若 backend 不实现 replay-store installer seam，server path 必须 fail-fast，而不是静默忽略

## TDD

### RED

1. 新增 FreePascal direct-library runtime test：
   - client default-config 应用 `ClientEarlyDataEnabled`
   - server default-config 应用 `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize`
   - server default-config 应用 replay-store file / directory
   - client path 拒绝 replay-store config
   - conflicting replay-store file + directory 被 reject
2. 新增跨 backend source contract：
   - 每个 backend library unit 都必须：
     - 先做 replay-store client-scope 校验
     - 再套 early-data context config
     - 再套 replay-store config

### GREEN

- 引入共享内部 helper，避免 4 个 backend 再各自复制 early-data / replay-store 逻辑
- 在 OpenSSL / FreePascal / WinSSL / MbedTLS / WolfSSL 的 direct-library `CreateContext(AType)` 上统一接入

### REGRESSION

- 复用 direct-library default-config parity contract
- 复用 direct-library ServerName compatibility contract
- `git diff --check`

## Files

- Add: `docs/plans/2026-05-18-direct-library-early-data-replay-store-parity.md`
- Add: `src/fafafa.ssl.context.config.pas`
- Add: `tests/test_direct_library_early_data_replay_store_parity.pas`
- Add: `tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh`
- Update: `src/fafafa.ssl.openssl.backed.pas`
- Update: `src/fafafa.ssl.freepascal.lib.pas`
- Update: `src/fafafa.ssl.winssl.lib.pas`
- Update: `src/fafafa.ssl.mbedtls.lib.pas`
- Update: `src/fafafa.ssl.wolfssl.lib.pas`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Commands

1. `bash -n tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh`
2. `bash tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh`
3. `mkdir -p tmp/test_direct_library_early_data_replay_store_parity && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_direct_library_early_data_replay_store_parity -FEtmp/test_direct_library_early_data_replay_store_parity -otmp/test_direct_library_early_data_replay_store_parity/test_direct_library_early_data_replay_store_parity tests/test_direct_library_early_data_replay_store_parity.pas && ./tmp/test_direct_library_early_data_replay_store_parity/test_direct_library_early_data_replay_store_parity`
4. `bash tests/scripts/test_direct_library_default_config_parity_contract.sh`
5. `bash tests/scripts/test_direct_library_servername_compatibility_contract.sh`
6. `git diff --check`

## Expected Outputs

- RED 时：
  - source contract 失败，证明 library path 还没统一接入 early-data / replay-store helper
  - FreePascal runtime test 失败，证明 direct-library path 还没把 early-data / replay-store truth 套到新 context
- GREEN 后：
  - direct-library path 和 factory/context path 在 early-data / replay-store 语义上保持同一套真相
  - FreePascal runtime test 通过
  - cross-backend source contract 通过
