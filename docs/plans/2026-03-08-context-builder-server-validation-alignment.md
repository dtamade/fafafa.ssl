# Context Builder Server Validation Alignment (2026-03-08)

## Goal
- 修复 `TSSLContextBuilderImpl` server 路径里 build / validation 的语义漂移。
- 保证 server builder 对 `PKCS#11`、`PEM` 和 `WithSystemRoots` 的行为与 validation 结论一致。
- 顺手记录下一层架构风险：`ISSLLibrary.CreateContext` 的 library-default 语义仍跨后端不一致。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_validation.pas`
- `tests/test_context_builder_backend_store_consistency.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Review
### 这波确认并修复的 server drift
1. `ValidateServer` 把私钥来源限定为 file/PEM，但 `BuildServer` 实际允许 `UsePKCS11(...)`。
2. `ValidateServer` 对 `WithSystemRoots` 仍报“无 CA”警告，但 `WithSystemRoots` 本身是 builder 的通用配置入口，server 路径本应支持它。
3. `BuildServer` 只处理 certificate/private-key file 和 PKCS#11，遗漏了 certificate/private-key PEM；这与 client 路径以及 validation 中“PEM will be used”的语义不一致。
4. `BuildServer` 在显式 backend 分支没有把 `SelectedBackend` 传递到后续 store 路径，导致 server path 无法像 client path 那样稳定复用同一 backend 的 store。

### 仍然保留的下一层架构问题
- `ISSLLibrary.CreateContext` 的“library default config 是否在 backend 内部生效”仍然跨后端不一致：
  - OpenSSL / WinSSL：backend `CreateContext` 内部会读取 `FDefaultConfig`
  - FreePascal / MbedTLS / WolfSSL：backend `CreateContext` 基本不应用 `FDefaultConfig`
- 这意味着 direct `ISSLLibrary.CreateContext(...)` 仍不是跨后端等价 API；当前只是在 factory/builder 路径上尽量收敛。

## RED
1. 在 `tests/config/test_config_validation.pas` 新增：
   - server + `UsePKCS11` 校验
   - server + `WithSystemRoots` 校验
2. 在 `tests/test_context_builder_backend_store_consistency.pas` 新增：
   - server + explicit backend + `WithSystemRoots`
   - server PEM 材料应实际装载到 context
3. 运行：
   - `fpc -Fu./src tests/config/test_config_validation.pas -otmp/test_config_validation && ./tmp/test_config_validation`
   - `fpc -Fu./src tests/test_context_builder_backend_store_consistency.pas -otmp/test_context_builder_backend_store_consistency && ./tmp/test_context_builder_backend_store_consistency`

## GREEN
1. 修改 `src/fafafa.ssl.context.builder.pas`
   - `ValidateServer` 接受 `FPKCS11URI` 作为合法私钥来源
   - `ValidateServer` 在 `FUseSystemRoots=True` 时不再误报无 CA 警告
   - `BuildServer` 补齐 certificate/private-key PEM 加载
   - `BuildServer` 在 server 路径也处理 `WithSystemRoots`
   - `BuildServer` 显式 backend 分支补齐 `SelectedBackend`
2. 让 `tests/config/test_config_validation.pas` 自带 `fafafa.ssl.freepascal.lib`，避免 build-with-validation 成功用例依赖外部注册状态

## Regression
- `fpc -Fu./src tests/config/test_config_validation.pas -otmp/test_config_validation && ./tmp/test_config_validation`
- `fpc -Fu./src tests/test_context_builder_backend_store_consistency.pas -otmp/test_context_builder_backend_store_consistency && ./tmp/test_context_builder_backend_store_consistency`
- `python3 scripts/compile_all_modules.py`
