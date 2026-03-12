# Request Config Logging Scope Visibleization (2026-03-09)

## Goal
- 让 `TSSLFactory.CreateContext(const AConfig)` 不再静默接受 library-scope 的 `LogLevel` / `LogCallback`。
- 明确 `TSSLConfig` 中日志字段的作用域：request-scoped factory path 禁止使用，library-default path 继续允许。
- 避免调用方误以为“给 `TSSLConfig` 填日志字段就能影响单次 context 创建”。

## Scope
- `src/fafafa.ssl.factory.pas`
- `tests/test_factory_request_config_logging_scope_visibleization.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
1. 保持 library-default path 行为不变：`ISSLLibrary.SetDefaultConfig(...)` 仍可承载 `LogLevel` / `LogCallback`。
2. 只在 request-scoped `TSSLFactory.CreateContext(const AConfig)` 上增加显式失败：
   - `LogLevel <> sslLogNone` 时抛 `ESSLConfigurationException`
   - `Assigned(LogCallback)` 时抛 `ESSLConfigurationException`
3. 继续复用已有 dead-field request guard，不把日志字段混进 runtime/library shared validator。

## RED
1. 新增 `tests/test_factory_request_config_logging_scope_visibleization.pas`
   - request path：`LogLevel := sslLogInfo` 必须抛 `ESSLConfigurationException`
   - request path：`LogCallback := @Probe.HandleLog` 必须抛 `ESSLConfigurationException`
   - library-default path：`SetDefaultConfig(...)` 仍允许保存自定义 `LogLevel` / `LogCallback`
2. 运行：
   - `fpc -Fu./src tests/test_factory_request_config_logging_scope_visibleization.pas -otmp/test_factory_request_config_logging_scope_visibleization && ./tmp/test_factory_request_config_logging_scope_visibleization`

## GREEN
1. 修改 `src/fafafa.ssl.factory.pas`
   - `ValidateRequestScopedConfigFields(...)` 在 request path 上追加日志字段作用域校验
2. 不改 backend `SetDefaultConfig(...)`
   - 保持 library-scope logging 作为显式支持能力

## Regression
- `fpc -Fu./src tests/test_factory_request_config_logging_scope_visibleization.pas -otmp/test_factory_request_config_logging_scope_visibleization && ./tmp/test_factory_request_config_logging_scope_visibleization`
- `fpc -Fu./src tests/test_factory_request_config_dead_field_visibleization.pas -otmp/test_factory_request_config_dead_field_visibleization_serial && ./tmp/test_factory_request_config_dead_field_visibleization_serial`
- `fpc -Fu./src tests/test_library_default_config_dead_field_visibleization.pas -otmp/test_library_default_config_dead_field_visibleization && ./tmp/test_library_default_config_dead_field_visibleization`
- `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race && ./tmp/test_factory_shared_config_and_init_race`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- src/fafafa.ssl.factory.pas tests/test_factory_request_config_logging_scope_visibleization.pas docs/plans/2026-03-09-request-config-logging-scope-visibleization.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md`

## Execution Log (2026-03-09)

### RED
- Added `tests/test_factory_request_config_logging_scope_visibleization.pas`.
- First RED run:
  - `fpc -Fu./src tests/test_factory_request_config_logging_scope_visibleization.pas -otmp/test_factory_request_config_logging_scope_visibleization && ./tmp/test_factory_request_config_logging_scope_visibleization`
  - Result: FAIL
  - Key failure: `request LogLevel should raise ESSLConfigurationException`

### GREEN
- Updated `src/fafafa.ssl.factory.pas`:
  - `ValidateRequestScopedConfigFields(...)` now rejects request-scoped `LogLevel` / `LogCallback` usage.
- Re-ran contract:
  - `fpc -Fu./src tests/test_factory_request_config_logging_scope_visibleization.pas -otmp/test_factory_request_config_logging_scope_visibleization && ./tmp/test_factory_request_config_logging_scope_visibleization`
  - Result: PASS

### Regression
- `fpc -Fu./src tests/test_factory_request_config_dead_field_visibleization.pas -otmp/test_factory_request_config_dead_field_visibleization` initially hit transient linker failure (`ld.bfd: final link failed: file truncated`) during parallel validation; serial rerun passed.
- `fpc -Fu./src tests/test_factory_request_config_dead_field_visibleization.pas -otmp/test_factory_request_config_dead_field_visibleization_serial && ./tmp/test_factory_request_config_dead_field_visibleization_serial` => PASS
- `fpc -Fu./src tests/test_library_default_config_dead_field_visibleization.pas -otmp/test_library_default_config_dead_field_visibleization && ./tmp/test_library_default_config_dead_field_visibleization` => PASS
- `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race && ./tmp/test_factory_shared_config_and_init_race` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)
- `git diff --check -- src/fafafa.ssl.factory.pas tests/test_factory_request_config_logging_scope_visibleization.pas docs/plans/2026-03-09-request-config-logging-scope-visibleization.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md` => PASS
