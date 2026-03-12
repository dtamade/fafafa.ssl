# Library Default Dead-Field Visibleization (2026-03-09)

## Goal
- 让 backend `ISSLLibrary.SetDefaultConfig(...)` 也不再静默接受 `TSSLConfig` 里未接线的 `BufferSize` / `HandshakeTimeout`。
- 与已完成的 factory request path visibleization 保持一致，避免“request path 会失败、library-default path 却继续静默吞掉”的分叉。
- 通过共享校验 helper 收口实现，避免各后端再次漂移。

## Scope
- `src/fafafa.ssl.factory.pas`
- `src/fafafa.ssl.openssl.backed.pas`
- `src/fafafa.ssl.winssl.lib.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `tests/test_library_default_config_dead_field_visibleization.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
1. 把 request-path 的 dead-field guard 抽成共享 helper：`TSSLFactory.ValidateRuntimeConfigFields(...)`。
2. `TSSLFactory.CreateContext(const AConfig)` 继续复用该 helper。
3. 各 backend `SetDefaultConfig(...)` 在 `NormalizeConfig(...)` 前调用同一个 helper，统一拒绝非默认的 `BufferSize` / `HandshakeTimeout`。
4. 仍允许 `0` 和默认值通过，以保留现有默认配置兼容性。

## RED
1. 新增 `tests/test_library_default_config_dead_field_visibleization.pas`
   - direct library path：`GetLibraryInstance -> SetDefaultConfig(...)`
   - 断言非默认 `HandshakeTimeout` / `BufferSize` 必须抛出 `ESSLConfigurationException`
   - 断言默认值仍允许通过
2. 运行：
   - `fpc -Fu./src tests/test_library_default_config_dead_field_visibleization.pas -otmp/test_library_default_config_dead_field_visibleization && ./tmp/test_library_default_config_dead_field_visibleization`

## GREEN
1. 修改 `src/fafafa.ssl.factory.pas`
   - 将 request-path guard 提升为共享 `TSSLFactory.ValidateRuntimeConfigFields(...)`
2. 修改 backend `SetDefaultConfig(...)`
   - `src/fafafa.ssl.openssl.backed.pas`
   - `src/fafafa.ssl.winssl.lib.pas`
   - `src/fafafa.ssl.freepascal.lib.pas`
   - `src/fafafa.ssl.mbedtls.lib.pas`
   - `src/fafafa.ssl.wolfssl.lib.pas`

## Regression
- `fpc -Fu./src tests/test_library_default_config_dead_field_visibleization.pas -otmp/test_library_default_config_dead_field_visibleization && ./tmp/test_library_default_config_dead_field_visibleization`
- `fpc -Fu./src tests/test_factory_request_config_dead_field_visibleization.pas -otmp/test_factory_request_config_dead_field_visibleization && ./tmp/test_factory_request_config_dead_field_visibleization`
- `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race && ./tmp/test_factory_shared_config_and_init_race`
- `fpc -Fu./src tests/test_library_create_context_default_config_consistency.pas -otmp/test_library_create_context_default_config_consistency && ./tmp/test_library_create_context_default_config_consistency`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- src/fafafa.ssl.factory.pas src/fafafa.ssl.openssl.backed.pas src/fafafa.ssl.winssl.lib.pas src/fafafa.ssl.freepascal.lib.pas src/fafafa.ssl.mbedtls.lib.pas src/fafafa.ssl.wolfssl.lib.pas tests/test_library_default_config_dead_field_visibleization.pas docs/plans/2026-03-09-library-default-dead-field-visibleization.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md`

## Execution Log (2026-03-09)

### RED
- Added `tests/test_library_default_config_dead_field_visibleization.pas`.
- First RED run:
  - `fpc -Fu./src tests/test_library_default_config_dead_field_visibleization.pas -otmp/test_library_default_config_dead_field_visibleization && ./tmp/test_library_default_config_dead_field_visibleization`
  - Result: FAIL
  - Key failure: `FreePascal Native default HandshakeTimeout should raise ESSLConfigurationException`

### GREEN
- Updated `src/fafafa.ssl.factory.pas`:
  - Exposed shared `TSSLFactory.ValidateRuntimeConfigFields(...)`.
  - `CreateContext(const AConfig)` continues to use the same shared validation entry.
- Updated backend `SetDefaultConfig(...)` implementations in:
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
- Re-ran new direct-library default-config contract:
  - `fpc -Fu./src tests/test_library_default_config_dead_field_visibleization.pas -otmp/test_library_default_config_dead_field_visibleization && ./tmp/test_library_default_config_dead_field_visibleization`
  - Result: PASS

### Regression
- `fpc -Fu./src tests/test_factory_request_config_dead_field_visibleization.pas -otmp/test_factory_request_config_dead_field_visibleization && ./tmp/test_factory_request_config_dead_field_visibleization` => PASS
- `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race && ./tmp/test_factory_shared_config_and_init_race` => PASS
- `fpc -Fu./src tests/test_library_create_context_default_config_consistency.pas -otmp/test_library_create_context_default_config_consistency && ./tmp/test_library_create_context_default_config_consistency` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)
- `git diff --check -- src/fafafa.ssl.factory.pas src/fafafa.ssl.openssl.backed.pas src/fafafa.ssl.winssl.lib.pas src/fafafa.ssl.freepascal.lib.pas src/fafafa.ssl.mbedtls.lib.pas src/fafafa.ssl.wolfssl.lib.pas tests/test_library_default_config_dead_field_visibleization.pas docs/plans/2026-03-09-library-default-dead-field-visibleization.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md` => PASS
