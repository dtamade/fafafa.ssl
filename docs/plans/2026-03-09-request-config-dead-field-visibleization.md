# Request Config Dead-Field Visibleization (2026-03-09)

## Goal
- 让 `TSSLFactory.CreateContext(const AConfig)` 不再静默接受 `TSSLConfig` 里未接线的 request-scoped 死字段。
- 先处理风险最高的 `BufferSize` / `HandshakeTimeout`，避免调用方误以为这些值会影响 context 创建行为。
- 保持兼容边界：默认值仍可通过，避免把 `CreateDefaultConfig(...)` 这类现有调用全部打断。

## Scope
- `src/fafafa.ssl.factory.pas`
- `tests/test_factory_request_config_dead_field_visibleization.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
1. request path 只在 `TSSLFactory.CreateContext(const AConfig)` 增加显式校验，不扩散到 builder 或 backend `SetDefaultConfig`。
2. 若 `BufferSize` / `HandshakeTimeout` 被设置为**非默认且非零**值，则立即抛出 `ESSLConfigurationException`，把“静默无效”变成“显式失败”。
3. 允许 `0` 和默认值通过，以兼容零初始化配置和 `CreateDefaultConfig(...)` 派生配置。

## RED
1. 新增 `tests/test_factory_request_config_dead_field_visibleization.pas`
   - `HandshakeTimeout := 100` 时，`TSSLFactory.CreateContext(LConfig)` 必须抛出 `ESSLConfigurationException`
   - `BufferSize := 8192` 时，`TSSLFactory.CreateContext(LConfig)` 必须抛出 `ESSLConfigurationException`
   - 默认值 `SSL_DEFAULT_BUFFER_SIZE` / `SSL_DEFAULT_HANDSHAKE_TIMEOUT` 仍允许通过
2. 运行：
   - `fpc -Fu./src tests/test_factory_request_config_dead_field_visibleization.pas -otmp/test_factory_request_config_dead_field_visibleization && ./tmp/test_factory_request_config_dead_field_visibleization`

## GREEN
1. 修改 `src/fafafa.ssl.factory.pas`
   - 新增 request-scoped dead-field guard helper
   - 在 `CreateContext(const AConfig)` 进入 runtime 前执行校验
2. 异常文案明确指出：
   - `BufferSize` 当前不会参与 context 创建
   - `HandshakeTimeout` 应改走 `ISSLConnection.SetTimeout` 或 connection builder timeout

## Regression
- `fpc -Fu./src tests/test_factory_request_config_dead_field_visibleization.pas -otmp/test_factory_request_config_dead_field_visibleization && ./tmp/test_factory_request_config_dead_field_visibleization`
- `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race && ./tmp/test_factory_shared_config_and_init_race`
- `fpc -Fu./src tests/test_library_create_context_default_config_consistency.pas -otmp/test_library_create_context_default_config_consistency && ./tmp/test_library_create_context_default_config_consistency`
- `fpc -Fu./src tests/test_factory_logic.pas -otmp/test_factory_logic && ./tmp/test_factory_logic`
- `fpc -Fu./src tests/test_context_builder_backend_store_consistency.pas -otmp/test_context_builder_backend_store_consistency && ./tmp/test_context_builder_backend_store_consistency`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- src/fafafa.ssl.factory.pas tests/test_factory_request_config_dead_field_visibleization.pas docs/plans/2026-03-09-request-config-dead-field-visibleization.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md`


## Execution Log (2026-03-09)

### RED
- Added `tests/test_factory_request_config_dead_field_visibleization.pas`.
- First RED run:
  - `fpc -Fu./src tests/test_factory_request_config_dead_field_visibleization.pas -otmp/test_factory_request_config_dead_field_visibleization && ./tmp/test_factory_request_config_dead_field_visibleization`
  - Result: FAIL
  - Key failure: `request HandshakeTimeout should raise ESSLConfigurationException`

### GREEN
- Updated `src/fafafa.ssl.factory.pas`:
  - Added `ValidateRequestScopedConfigFields(...)`.
  - `CreateContext(const AConfig)` now rejects non-default `BufferSize` / `HandshakeTimeout` before runtime context creation.
- Re-ran contract:
  - `fpc -Fu./src tests/test_factory_request_config_dead_field_visibleization.pas -otmp/test_factory_request_config_dead_field_visibleization && ./tmp/test_factory_request_config_dead_field_visibleization`
  - Result: PASS

### Regression
- `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race && ./tmp/test_factory_shared_config_and_init_race` => PASS
- `fpc -Fu./src tests/test_library_create_context_default_config_consistency.pas -otmp/test_library_create_context_default_config_consistency && ./tmp/test_library_create_context_default_config_consistency` => PASS
- `fpc -Fu./src tests/test_factory_logic.pas -otmp/test_factory_logic && ./tmp/test_factory_logic` => PASS
- `fpc -Fu./src tests/test_context_builder_backend_store_consistency.pas -otmp/test_context_builder_backend_store_consistency && ./tmp/test_context_builder_backend_store_consistency` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)
- `git diff --check -- src/fafafa.ssl.factory.pas tests/test_factory_request_config_dead_field_visibleization.pas docs/plans/2026-03-09-request-config-dead-field-visibleization.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md` => PASS
