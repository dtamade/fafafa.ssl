# 2026-05-21 CAFile CAPath Trust-Loading Parity

## Goal

补齐 `CAFile` / `CAPath` 在 `fafafa.ssl` 三条 context 创建路径上的真实消费逻辑，避免 public config surface 看起来完整，但实际只有一部分入口真正加载 trust anchors：

- one-shot factory request path
  - `TSSLFactory.CreateContext(const AConfig)`
- raw factory default-config path
  - `ISSLLibrary.SetDefaultConfig(...)` + `TSSLFactory.CreateContext(AType, ALibType)`
- direct-library default-config path
  - `ISSLLibrary.SetDefaultConfig(...)` + `ISSLLibrary.CreateContext(AType)`

同时把 active docs 里 direct-library default-config 的字段真相补齐，避免继续遗漏 `CAFile` / `CAPath`。

## Architecture

- factory context paths
  - `src/fafafa.ssl.factory.pas`
- direct-library context paths
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
- active docs truth
  - `docs/reference/API_REFERENCE.md`
- focused verification
  - `tests/contract/test_cafile_capath_trust_loading_parity_entry.pas`
  - `tests/scripts/test_cafile_capath_trust_loading_parity_contract.sh`
  - `tests/test_freepascal_client_chain_trust_runtime.pas`

## TDD

### RED

1. 新增 focused source contract，锁住：
   - one-shot factory path 必须消费 `CAPath`
   - 五个 backend direct-library path 必须消费 `CAFile` / `CAPath`
   - API reference 的 direct-library aligned-fields truth 必须包含 `CAFile` / `CAPath`
2. 新增 mock-based Pascal contract，验证：
   - one-shot factory request path 会把 `CAFile` / `CAPath` 真正传给 context
   - raw factory default-config path 至少会通过 backend default-config 消费 `CAFile` / `CAPath`
   - direct-library default-config path 会消费 `CAFile` / `CAPath`
   - server `VerifyMode=[sslVerifyPeer]` 在存在 `CAFile` / `CAPath` 时不会被降成 no-verify
3. 扩展 real FreePascal trust runtime，验证：
   - one-shot factory config path 的 `CAPath` 真正生效
   - raw factory default-config path 的 `CAFile` / `CAPath` 真正生效
   - direct-library default-config path 的 `CAFile` / `CAPath` 真正生效

### GREEN

- `TSSLFactory.CreateContext(const AConfig)` 补齐 `LoadCAPath(...)`
- 五个 backend `CreateContext(AType)` 补齐 `LoadCAFile(...)` / `LoadCAPath(...)`
- API reference 里 direct-library aligned fields 补齐 `CAFile` / `CAPath`

### REGRESSION

- focused source contract
- focused Pascal contract
- focused FreePascal trust runtime
- existing direct-library default-config parity test
- `git diff --check`

## Files

- Add: `docs/plans/2026-05-21-cafile-capath-trust-loading-parity.md`
- Add: `tests/contract/test_cafile_capath_trust_loading_parity_entry.pas`
- Add: `tests/scripts/test_cafile_capath_trust_loading_parity_contract.sh`
- Update: `src/fafafa.ssl.factory.pas`
- Update: `src/fafafa.ssl.openssl.backed.pas`
- Update: `src/fafafa.ssl.freepascal.lib.pas`
- Update: `src/fafafa.ssl.mbedtls.lib.pas`
- Update: `src/fafafa.ssl.wolfssl.lib.pas`
- Update: `src/fafafa.ssl.winssl.lib.pas`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `tests/test_freepascal_client_chain_trust_runtime.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Commands

1. `bash -n tests/scripts/test_cafile_capath_trust_loading_parity_contract.sh`
2. `bash tests/scripts/test_cafile_capath_trust_loading_parity_contract.sh`
3. `mkdir -p tmp/cafile_capath_trust_loading_parity && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/cafile_capath_trust_loading_parity -FEtmp/cafile_capath_trust_loading_parity -otmp/cafile_capath_trust_loading_parity/test_cafile_capath_trust_loading_parity tests/contract/test_cafile_capath_trust_loading_parity_entry.pas && ./tmp/cafile_capath_trust_loading_parity/test_cafile_capath_trust_loading_parity`
4. `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime`
5. `mkdir -p tmp/test_direct_library_default_config_parity && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_direct_library_default_config_parity -FEtmp/test_direct_library_default_config_parity -otmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity tests/test_direct_library_default_config_parity.pas && ./tmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity`
6. `git diff --check`

## Expected Outcome

- one-shot factory request path 不再只支持 `CAFile`
- raw factory 与 direct-library default-config path 不再把 `CAFile` / `CAPath` 静默丢掉
- active docs 不再遗漏 direct-library 对齐字段
- FreePascal real runtime 可以直接证明这条 trust-loading family 已经穿透到真实握手

## Outcome

- PASS
- `bash tests/scripts/test_cafile_capath_trust_loading_parity_contract.sh` 在当前 head 上通过。
- `bash tests/scripts/test_direct_library_default_config_parity_contract.sh` 继续通过。
- `TSSLFactory.CreateContext(const AConfig)` 与五个 backend 的 direct-library `CreateContext(AType)` 现在都稳定消费 `CAFile` / `CAPath`。
