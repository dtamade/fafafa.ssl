# Factory Library Registration Constructor Fix (2026-03-09)

## Goal
- 修复 `TSSLFactory` 通过 class-only registration 创建内建 backend 时丢失 constructor 基线的问题。
- 让工厂缓存实例与 direct `Create*Library` 一样保留 backend constructor 初始化的 `FDefaultConfig` 等默认状态。
- 把仓库内依赖 constructor 基线的 fake library 注册也收敛到显式 `LibraryFactory`，减少后续测试误判。

## Scope
- `src/fafafa.ssl.factory.pas`
- `src/fafafa.ssl.openssl.backed.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `src/fafafa.ssl.winssl.lib.pas`
- `tests/test_factory_backend_default_config_initialization.pas`
- `tests/test_factory_shared_config_and_init_race.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
1. 在 `TSSLLibraryRegistration` 中加入可选 `LibraryFactory`。
2. `TSSLFactory.CreateLibraryInstance(...)` 优先调用显式 `LibraryFactory`，仅把 class-only path 保留为兼容 fallback。
3. 所有内建 backend 注册同时提供：
   - `ALibraryClass`（保留现有注册信息）
   - `ALibraryFactory`（确保真正走各 backend 的 `Create*Library` 入口）
4. 仓库内依赖 constructor 基线的 fake backend 也改为显式 factory 注册，避免测试继续隐式依赖 class-only path。

## RED
1. 新增 `tests/test_factory_backend_default_config_initialization.pas`
   - 对 `OpenSSL` / `MbedTLS` / `WolfSSL` / `FreePascal` 分别比较：
     - direct `Create*Library.GetDefaultConfig`
     - `TSSLFactory.GetLibrary(...).GetDefaultConfig`
   - 断言 factory 实例必须保留 constructor baseline 的 `LibraryType` / `VerifyDepth` / `SessionTimeout` / `ProtocolVersions` / `LogLevel`。
2. RED 现象（修复前）
   - factory 路径拿到的默认配置接近全零：`cfg.lib=0`, `verifyDepth=0`, `sessTimeout=0`, `log=0`
   - direct create 路径则能保留 backend constructor 基线

## GREEN
1. 修改 `src/fafafa.ssl.factory.pas`
   - 增加 `TSSLLibraryFactoryFunc`
   - `RegisterLibrary(...)` 支持 `ALibraryFactory`
   - `CreateLibraryInstance(...)` 优先使用 `LibraryFactory`
2. 修改内建 backend 注册点
   - `src/fafafa.ssl.openssl.backed.pas`
   - `src/fafafa.ssl.freepascal.lib.pas`
   - `src/fafafa.ssl.mbedtls.lib.pas`
   - `src/fafafa.ssl.wolfssl.lib.pas`
   - `src/fafafa.ssl.winssl.lib.pas`
3. 收口测试 fake backend 注册
   - `tests/test_factory_shared_config_and_init_race.pas` 中依赖 constructor baseline 的 fake library 改为显式 factory 注册

## Verification
- `fpc -Fu./src -otmp/test_factory_backend_default_config_initialization tests/test_factory_backend_default_config_initialization.pas && ./tmp/test_factory_backend_default_config_initialization`
- `fpc -Fu./src -otmp/test_factory_shared_config_and_init_race tests/test_factory_shared_config_and_init_race.pas && ./tmp/test_factory_shared_config_and_init_race`
- `fpc -Fu./src -otmp/test_factory tests/test_factory.pas && ./tmp/test_factory`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- src/fafafa.ssl.factory.pas src/fafafa.ssl.openssl.backed.pas src/fafafa.ssl.freepascal.lib.pas src/fafafa.ssl.mbedtls.lib.pas src/fafafa.ssl.wolfssl.lib.pas src/fafafa.ssl.winssl.lib.pas tests/test_factory_backend_default_config_initialization.pas tests/test_factory_shared_config_and_init_race.pas docs/plans/2026-03-09-factory-library-registration-constructor-fix.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md`

## Notes
- 当前仓库里的高风险点已经收敛：内建 backend 与依赖 constructor baseline 的 fake backend 都不再走 class-only instantiation。
- `RegisterLibrary(..., ALibraryClass)` 仍保留兼容 fallback；第三方 backend 若把关键默认状态写在 constructor 中，仍应优先传入 `ALibraryFactory`。
- 剩余 class-only 注册主要在测试 fake library，当前不依赖 constructor 基线，风险较低，但后续仍建议补文档/弃用信号。
