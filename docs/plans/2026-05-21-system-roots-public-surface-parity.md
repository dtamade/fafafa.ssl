# System-Roots Public Surface Parity

## Goal

补齐 `UseSystemRoots` 在 `fafafa.ssl` 公共接口中的真实缺口，让下面三条路径都能表达并落实同一条 system-roots opt-in：

- `TSSLConfig` + `TSSLFactory.CreateContext(...)`
- `ISSLLibrary.SetDefaultConfig(...)` + `TSSLFactory.CreateContext(AType, ALibType)`
- `ISSLLibrary.SetDefaultConfig(...)` + `ISSLLibrary.CreateContext(AType)`

同时把 server verify baseline 的 trust-root 判定同步到这条能力上，避免 server `VerifyMode=[sslVerifyPeer] + UseSystemRoots=True` 仍被误降成 no-verify。

## Architecture

- public config surface
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.pas`
- factory context paths
  - `src/fafafa.ssl.factory.pas`
- direct-library context paths
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
- docs/debug truth
  - `src/fafafa.ssl.debug.utils.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/ARCHITECTURE.md`
  - `docs/CA_CERTIFICATE_AUTO_LOADING.md`
  - `docs/guides/GETTING_STARTED.md`

## TDD

### RED

1. 新增 mock-based Pascal contract：
   - one-shot factory config path
   - factory raw/default-config path
   - direct-library default-config path
2. 覆盖 client/server 两侧，并验证：
   - `CreateCertificateStore`
   - `LoadSystemStore`
   - `SetCertificateStore`
   - server `VerifyMode=[sslVerifyPeer]` 在 `UseSystemRoots=True` 时不再被清空
3. 新增 source/docs contract：
   - `TSSLConfig` 必须正式声明 `UseSystemRoots`
   - factory 与 direct-library path 必须都有 system-roots plumbing
   - active docs 必须把 builder/config/direct-library 三条 opt-in truth 讲一致

### GREEN

- 给 `TSSLConfig` 增加 `UseSystemRoots: Boolean`
- factory 两个 `CreateContext` overload 在请求时加载 system roots
- `ResolveContextVerifyModeForCreation(...)` 把 `UseSystemRoots` 视为存在 trust roots
- 五个 backend direct-library `CreateContext(AType)` 都补齐 system-roots store 注入
- `DumpSSLConfig` / 活跃 docs 同步这条 public truth

### REGRESSION

- focused contract
- `test_default_config`
- `test_config_validation`
- `git diff --check`

## Files

- Add: `docs/plans/2026-05-21-system-roots-public-surface-parity.md`
- Add: `tests/contract/test_system_roots_public_surface_entry.pas`
- Add: `tests/scripts/test_system_roots_public_surface_contract.sh`
- Update: `src/fafafa.ssl.base.pas`
- Update: `src/fafafa.ssl.pas`
- Update: `src/fafafa.ssl.factory.pas`
- Update: `src/fafafa.ssl.openssl.backed.pas`
- Update: `src/fafafa.ssl.freepascal.lib.pas`
- Update: `src/fafafa.ssl.mbedtls.lib.pas`
- Update: `src/fafafa.ssl.wolfssl.lib.pas`
- Update: `src/fafafa.ssl.winssl.lib.pas`
- Update: `src/fafafa.ssl.debug.utils.pas`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `docs/reference/ARCHITECTURE.md`
- Update: `docs/CA_CERTIFICATE_AUTO_LOADING.md`
- Update: `docs/guides/GETTING_STARTED.md`
- Update: `tests/config/test_default_config.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Commands

1. `bash -n tests/scripts/test_system_roots_public_surface_contract.sh`
2. `bash tests/scripts/test_system_roots_public_surface_contract.sh`
3. `mkdir -p tmp/system_roots_public_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/system_roots_public_surface -FEtmp/system_roots_public_surface -otmp/system_roots_public_surface/test_system_roots_public_surface tests/contract/test_system_roots_public_surface_entry.pas && ./tmp/system_roots_public_surface/test_system_roots_public_surface`
4. `mkdir -p tmp/test_default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_default_config -FEtmp/test_default_config -otmp/test_default_config/test_default_config tests/config/test_default_config.pas && ./tmp/test_default_config/test_default_config`
5. `mkdir -p tmp/config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_validation -FEtmp/config_validation -otmp/config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/config_validation/test_config_validation`
6. `git diff --check`

## Expected Outputs

- RED 时：
  - source/docs contract 失败
  - Pascal contract 编译失败或运行失败
- GREEN 后：
  - factory/direct-library system-roots opt-in 对齐
  - server verify baseline 在 `UseSystemRoots=True` 时保留 verify-peer
  - focused verification 全绿
