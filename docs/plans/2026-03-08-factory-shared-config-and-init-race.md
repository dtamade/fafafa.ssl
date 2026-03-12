# Factory Shared Config + Init Race (2026-03-08)

## Goal
- 修复 `TSSLFactory.CreateContext(const AConfig)` 通过 `SetDefaultConfig` 污染后端共享默认配置的问题。
- 修复 `TSSLFactory.GetLibrary` 首次获取同一后端时可能触发并发重复 `Initialize` 的竞态。

## Scope
- `src/fafafa.ssl.factory.pas`
- `tests/test_factory_shared_config_and_init_race.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
- 共享默认配置修复：`CreateContext(const AConfig)` 仅创建 context 并把请求配置应用到该 context，不再修改 `ISSLLibrary` 级默认配置。
- 初始化竞态修复：把“是否需要初始化 + 初始化动作”收敛到工厂锁保护下，保证同一后端实例最多只初始化一次。
- 保持公共 API 与调用方式不变，只收紧内部语义。

## RED
1. 新增 `tests/test_factory_shared_config_and_init_race.pas`
   - 契约 1：`CreateContext(AConfig)` 不得改变 `GetDefaultConfig` 的基线值。
   - 契约 2：并发 `GetLibrary` 时同一后端 `Initialize` 只执行一次。
2. 运行：
   - `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race && ./tmp/test_factory_shared_config_and_init_race`

## GREEN
1. 修改 `src/fafafa.ssl.factory.pas`
   - 去掉 `CreateContext(const AConfig)` 对 `SetDefaultConfig` 的依赖。
   - 收紧 `GetLibrary` 初始化临界区，避免重复初始化。

## Regression
- `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race && ./tmp/test_factory_shared_config_and_init_race`
- `fpc -Fu./src tests/test_context_builder_backend_store_consistency.pas -otmp/test_context_builder_backend_store_consistency && ./tmp/test_context_builder_backend_store_consistency`
- `fpc -Fu./src tests/test_factory_logic.pas -otmp/test_factory_logic && ./tmp/test_factory_logic`
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Execution Log (2026-03-08)

### RED
- Added `tests/test_factory_shared_config_and_init_race.pas`.
- First RED run:
  - `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race && ./tmp/test_factory_shared_config_and_init_race`
  - Result: FAIL
  - Key failure: `baseline session timeout mismatch: expected=777 actual=123`

### GREEN
- Updated `src/fafafa.ssl.factory.pas`:
  - `CreateContext(const AConfig)` no longer writes request config into `ISSLLibrary.SetDefaultConfig`.
  - `GetLibrary` initialization now happens inside the factory critical section.
  - `IsLibraryAvailable` uses the same tightened instance/init path, avoiding the same duplicate-init pattern.
- Re-ran contract:
  - `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race && ./tmp/test_factory_shared_config_and_init_race`
  - Result: PASS

### Regression
- `fpc -Fu./src tests/test_context_builder_backend_store_consistency.pas -otmp/test_context_builder_backend_store_consistency && ./tmp/test_context_builder_backend_store_consistency` => PASS
- `fpc -Fu./src tests/test_factory_logic.pas -otmp/test_factory_logic && ./tmp/test_factory_logic` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)
- `bash scripts/run_minimal_ci_gate.sh --fast-local` => PASS
