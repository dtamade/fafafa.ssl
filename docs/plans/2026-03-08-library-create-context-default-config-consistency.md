# Library CreateContext Default Config Consistency (2026-03-08)

## Goal
- 统一 direct `ISSLLibrary.CreateContext(...)` 的 library-default 语义，消除 FreePascal / MbedTLS / WolfSSL 与 OpenSSL / WinSSL 的跨后端分叉。
- 保持 `TSSLFactory.CreateContext(const AConfig)` 的 P0 修复不回退：请求级配置只落到 context，不再污染 library 级默认配置。
- 为后续 backend context/default-validation 架构复审缩小问题面。

## Scope
- `src/fafafa.ssl.factory.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `src/fafafa.ssl.winssl.lib.pas`
- `src/fafafa.ssl.openssl.backed.pas`
- `tests/test_library_create_context_default_config_consistency.pas`
- `tests/test_factory_shared_config_and_init_race.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
1. 抽出共享 helper：`TSSLFactory.ApplyConfigToContext(...)` 负责把 `TSSLConfig` 的 context 级字段统一应用到 `ISSLContext`。
2. direct library path 收敛：所有后端的 `ISSLLibrary.CreateContext(...)` 在创建 context 后都读取各自 `FDefaultConfig`，再调用同一个 helper。
3. factory request path 保持隔离：`TSSLFactory.CreateContext(const AConfig)` 只创建 context 并对该 context 应用请求配置，不写回 library 默认配置。
4. `TSSLConfig` 只使用真实存在的字段：证书/私钥 file 与 CA file/path；PEM 材料继续留在 context builder 路径处理。

## RED
1. 新增 `tests/test_library_create_context_default_config_consistency.pas`
   - direct library path：`GetLibraryInstance -> SetDefaultConfig -> LLib.CreateContext(...)`
   - 断言 session timeout / cache size / verify depth / ALPN / cipher list / cipher suites / verify mode / protocols / preferred version / server name 一致。
2. 运行：
   - `fpc -Fu./src tests/test_library_create_context_default_config_consistency.pas -otmp/test_library_create_context_default_config_consistency && ./tmp/test_library_create_context_default_config_consistency`

## GREEN
1. 修改各 backend `CreateContext(...)`
   - FreePascal / MbedTLS / WolfSSL 补齐 `FDefaultConfig -> ApplyConfigToContext`
   - OpenSSL / WinSSL 改为复用同一个 helper，避免后续再分叉
2. 修改 `src/fafafa.ssl.factory.pas`
   - `CreateContext(const AConfig)` 保持 request-only apply
   - 删除误加的 `CertificatePEM` / `PrivateKeyPEM` 字段引用，避免把 builder 语义混入 `TSSLConfig`
3. 调整 fake backend
   - `tests/test_factory_shared_config_and_init_race.pas` 中 fake library 也经由 helper 应用默认配置，跟真实后端语义保持一致

## Regression
- `fpc -Fu./src tests/test_library_create_context_default_config_consistency.pas -otmp/test_library_create_context_default_config_consistency && ./tmp/test_library_create_context_default_config_consistency`
- `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race && ./tmp/test_factory_shared_config_and_init_race`
- `fpc -Fu./src tests/test_context_builder_backend_store_consistency.pas -otmp/test_context_builder_backend_store_consistency && ./tmp/test_context_builder_backend_store_consistency`
- `fpc -Fu./src tests/config/test_config_validation.pas -otmp/test_config_validation && ./tmp/test_config_validation`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- src/fafafa.ssl.factory.pas src/fafafa.ssl.freepascal.lib.pas src/fafafa.ssl.mbedtls.lib.pas src/fafafa.ssl.wolfssl.lib.pas src/fafafa.ssl.winssl.lib.pas src/fafafa.ssl.openssl.backed.pas tests/test_library_create_context_default_config_consistency.pas tests/test_factory_shared_config_and_init_race.pas`

## Execution Log (2026-03-08)

### RED
- Added `tests/test_library_create_context_default_config_consistency.pas`.
- First RED run:
  - `fpc -Fu./src tests/test_library_create_context_default_config_consistency.pas -otmp/test_library_create_context_default_config_consistency && ./tmp/test_library_create_context_default_config_consistency`
  - Result: FAIL
  - Key failure: `FreePascal Native session timeout expected=123 actual=300`

### GREEN
- Updated `src/fafafa.ssl.factory.pas`:
  - Added shared `TSSLFactory.ApplyConfigToContext(...)` helper as the context-level application point.
  - Kept `CreateContext(const AConfig)` on the request path without mutating backend defaults.
  - Removed invalid `TSSLConfig.CertificatePEM` / `TSSLConfig.PrivateKeyPEM` references.
- Updated backend `CreateContext(...)` implementations in:
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.openssl.backed.pas`
- Updated `tests/test_factory_shared_config_and_init_race.pas` fake library to apply defaults through the shared helper.
- Re-ran new direct-library consistency contract:
  - `fpc -Fu./src tests/test_library_create_context_default_config_consistency.pas -otmp/test_library_create_context_default_config_consistency && ./tmp/test_library_create_context_default_config_consistency`
  - Result: PASS

### Regression
- `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race && ./tmp/test_factory_shared_config_and_init_race` => PASS
- `fpc -Fu./src tests/test_context_builder_backend_store_consistency.pas -otmp/test_context_builder_backend_store_consistency && ./tmp/test_context_builder_backend_store_consistency` => PASS
- `fpc -Fu./src tests/config/test_config_validation.pas -otmp/test_config_validation && ./tmp/test_config_validation` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)
- `git diff --check -- src/fafafa.ssl.factory.pas src/fafafa.ssl.freepascal.lib.pas src/fafafa.ssl.mbedtls.lib.pas src/fafafa.ssl.wolfssl.lib.pas src/fafafa.ssl.winssl.lib.pas src/fafafa.ssl.openssl.backed.pas tests/test_library_create_context_default_config_consistency.pas tests/test_factory_shared_config_and_init_race.pas` => PASS
