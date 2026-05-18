# Direct-Library Default-Config Parity

## Goal

修正 `ISSLLibrary.CreateContext(AType)` 在各 backend 上对 library default config 的套用漂移：

- 保持 `TSSLFactory.CreateContext(...)` 既有行为不变
- 让 direct-library path 不再只在 OpenSSL 上比较“完整”
- 用一个本机可运行的 runtime test 加一个跨 backend source contract 同时收口

## Architecture

- 把本批边界限定在 direct-library path：
  - `ISSLLibrary.SetDefaultConfig(...)`
  - `ISSLLibrary.CreateContext(AType)`
- 当前先对齐这组“已在 `ISSLContext` 上有公共 setter/getter 且被 factory path 明确消费”的默认配置：
  - `ProtocolVersions`
  - `PreferredVersion`
  - `VerifyMode`
  - `VerifyDepth`
  - `CipherList`
  - `CipherSuites`
  - `Options`
  - `SessionCacheSize`
  - `SessionTimeout`
  - `SessionCacheMode`
  - `ALPNProtocols`
- 这批先不扩到：
  - `ServerName` compatibility warning/reject parity
  - early-data / replay-store direct-library parity
  - `ISSLConnection` surface slimming

## TDD

### RED

1. 新增一个 FreePascal direct-library focused runtime test：
   - 修改 `ISSLLibrary` default config
   - 调 `Lib.CreateContext(sslCtxClient)`
   - 断言 context 真实反映 default config
2. 新增一个跨 backend source contract：
   - `SetDefaultConfig(...)` 必须先归一化 `TSSLConfig`
   - `CreateContext(AType)` 必须显式套用上面的 context-safe 默认字段

### GREEN

- 在 `freepascal` / `winssl` / `mbedtls` / `wolfssl` library units 中补齐：
  - `SetDefaultConfig(...)` normalization
  - `CreateContext(AType)` default-config apply block

### REGRESSION

- 继续复用已有 focused tests，确认没有把 factory 语义拖偏

## Files

- Add: `docs/plans/2026-05-18-direct-library-default-config-parity.md`
- Add: `tests/test_direct_library_default_config_parity.pas`
- Add: `tests/scripts/test_direct_library_default_config_parity_contract.sh`
- Update: `src/fafafa.ssl.freepascal.lib.pas`
- Update: `src/fafafa.ssl.winssl.lib.pas`
- Update: `src/fafafa.ssl.mbedtls.lib.pas`
- Update: `src/fafafa.ssl.wolfssl.lib.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`

## Commands

1. `bash -n tests/scripts/test_direct_library_default_config_parity_contract.sh`
2. `bash tests/scripts/test_direct_library_default_config_parity_contract.sh`
3. `mkdir -p tmp/test_direct_library_default_config_parity && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_direct_library_default_config_parity -FEtmp/test_direct_library_default_config_parity -otmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity tests/test_direct_library_default_config_parity.pas && ./tmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity`
4. `mkdir -p tmp/test_factory_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_connection_scope_clarification -FEtmp/test_factory_connection_scope_clarification -otmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification tests/test_factory_connection_scope_clarification.pas && ./tmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification`
5. `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
6. `git diff --check`

## Expected Outputs

- RED 时：
  - FreePascal direct-library runtime test 失败
  - source contract 失败
- GREEN 后：
  - direct-library runtime test 通过
  - contract 证明四个 backend 的 `SetDefaultConfig/CreateContext` parity 已补齐
  - 既有 factory scope-focused tests 继续为绿
