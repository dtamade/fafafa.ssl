# CreateDefaultConfig Request-Safe Logging Compatibility (2026-03-09)

## Goal
- 让 `CreateDefaultConfig(...)` 的返回值始终保持 request-path 可接受语义。
- 即使 backend library-default 已显式自定义 `LogLevel` / `LogCallback`，也不要把这些 library-scope 字段泄漏回 request config。
- 把当前环境里“测试偶然为绿”的行为收敛成源码层显式契约。

## Scope
- `src/fafafa.ssl.pas`
- `tests/config/test_default_config.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
1. `CreateDefaultConfig(...)` 继续从 backend 默认配置继承 context 级安全基线。
2. 在函数公共出口显式清理 request-path 不应携带的 library-scope 字段：
   - `Result.LogLevel := sslLogNone`
   - `Result.LogCallback := nil`
3. RED 合同不依赖环境偶然值：先把 `sslFreePascal` 的 library-default logging 改成自定义值，再断言 `CreateDefaultConfig(...)` 仍返回清理后的 request-safe config。

## RED
1. 扩展 `tests/config/test_default_config.pas`
   - 新增 `TestDefaultConfigIgnoresLibraryScopedLoggingDefaults`
   - 步骤：把默认 backend 暂时切到 `sslFreePascal`，对 `ISSLLibrary.SetDefaultConfig(...)` 注入 `LogLevel` / `LogCallback`，再调用 `CreateDefaultConfig(...)`
   - 断言：返回值必须仍为 `sslLogNone` / `nil`
2. 运行：
   - `fpc -B -Fu./src tests/config/test_default_config.pas -otmp/test_default_config_red && ./tmp/test_default_config_red`

## GREEN
1. 修改 `src/fafafa.ssl.pas`
   - 在 `CreateDefaultConfig(...)` 的公共出口显式清理 `LogLevel` / `LogCallback`
   - 保留其它 context 级默认值继承逻辑不变
2. 保持 fallback 分支兼容
   - 依然生成完整安全默认值
   - 但不再把 fallback logging 默认值暴露给 request config

## Verification
- `fpc -B -Fu./src tests/config/test_default_config.pas -otmp/test_default_config && ./tmp/test_default_config`
- `fpc -B -Fu./src tests/test_factory_request_config_logging_scope_visibleization.pas -otmp/test_factory_request_config_logging_scope_visibleization && ./tmp/test_factory_request_config_logging_scope_visibleization`
- `fpc -B -Fu./src tests/test_factory_request_config_dead_field_visibleization.pas -otmp/test_factory_request_config_dead_field_visibleization_serial && ./tmp/test_factory_request_config_dead_field_visibleization_serial`
- `fpc -B -Fu./src tests/test_library_default_config_dead_field_visibleization.pas -otmp/test_library_default_config_dead_field_visibleization && ./tmp/test_library_default_config_dead_field_visibleization`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- src/fafafa.ssl.pas tests/config/test_default_config.pas docs/plans/2026-03-09-create-default-config-request-safe-logging.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md`

## Notes
- 现场探针显示：当前 Linux 环境下，`TSSLFactory.GetLibrary(sslAutoDetect).GetDefaultConfig` 在已初始化缓存实例上返回的 logging 字段已经是 request-safe 值，但 direct `CreateMbedTLSLibrary` / `CreateOpenSSLLibrary` 仍保留 raw backend 默认值。
- 本波先把对外契约在 `CreateDefaultConfig(...)` 边界显式化；更深一层的 backend/default-config 可见性差异，留到后续架构复审处理。
