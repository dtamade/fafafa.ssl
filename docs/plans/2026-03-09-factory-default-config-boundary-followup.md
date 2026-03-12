# Factory Default-Config Boundary Follow-up Plan

**Goal**
- 收紧 `ISSLLibrary.SetDefaultConfig` 的边界，显式拒绝 request-only material fields，避免 library default config 暴露 backend `CreateContext(...)` 实际不会消费的 dead fields。

**Architecture**
- 先在 `tests/test_library_default_config_dead_field_visibleization.pas` 补 RED，证明 `CertificateFile` / `PrivateKeyFile` / `PrivateKeyPassword` / `CAFile` / `CAPath` 当前可以写入 library defaults。
- 再在 factory 增加 library-default scoped validator，由各 backend `SetDefaultConfig(...)` 统一调用。
- 保持 request path `TSSLFactory.CreateContext(const AConfig)` 对这些 material fields 的支持不变，只收紧 library default path。

**Files**
- Modify: `src/fafafa.ssl.factory.pas`
- Modify: `src/fafafa.ssl.openssl.backed.pas`
- Modify: `src/fafafa.ssl.freepascal.lib.pas`
- Modify: `src/fafafa.ssl.mbedtls.lib.pas`
- Modify: `src/fafafa.ssl.wolfssl.lib.pas`
- Modify: `src/fafafa.ssl.winssl.lib.pas`
- Modify: `tests/test_library_default_config_dead_field_visibleization.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`
- Update: `docs/plans/2026-03-current-summary.md`

**Steps**
1. 写 RED：library default config 不应接受 request-only certificate/CA material fields。
2. 跑 focused test，确认 dead-field visibleization 仍存在。
3. 增加 shared library-default validator，并接入各 backend `SetDefaultConfig(...)`。
4. 跑 focused suites + `python3 -u scripts/compile_all_modules.py`。
5. 回写 working memory 与月度索引。

**Expected Outputs**
- `ISSLLibrary.SetDefaultConfig` 对 request-only material fields 显式报错。
- request path `TSSLFactory.CreateContext(const AConfig)` 保持可用。
- backend default-config boundary 从“隐式无效”变成“显式可见”。
