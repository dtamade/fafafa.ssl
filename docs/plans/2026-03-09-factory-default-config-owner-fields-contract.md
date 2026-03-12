# Factory Default-Config Owner Fields Contract Plan

**Goal**
- 收口 `ISSLLibrary.SetDefaultConfig` 中 `LibraryType` / `ContextType` 的 owner-field 语义：允许调用方复用整份 `TSSLConfig`，但库级默认配置必须把 owner fields 归一化为 backend-owned stable values，而不是把它们当作可自由持久化输入。

**Architecture**
- 先在 `tests/test_library_default_config_dead_field_visibleization.pas` 增加 RED，证明 library defaults 当前会保留调用方传入的错 backend / server context owner fields。
- 再在 factory 增加 shared owner-field normalize helper，由各 backend `SetDefaultConfig(...)` 统一调用。
- 其余默认字段保持可配置；request path `TSSLFactory.CreateContext(const AConfig)` 继续按调用方的 `LibraryType` / `ContextType` 工作。

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
1. 写 RED：library default config 的 `LibraryType` / `ContextType` 不应保留调用方传入的 owner-field 噪音。
2. 跑 focused test，确认当前仍会把错 backend / server context 持久化到 `GetDefaultConfig`。
3. 增加 shared normalize helper，并接入各 backend `SetDefaultConfig(...)`。
4. 跑 focused factory/default-config suites + `python3 -u scripts/compile_all_modules.py`。
5. 回写 working memory 与月度索引。

**Expected Outputs**
- `GetDefaultConfig` 始终回到 backend-owned `LibraryType` 与 stable `ContextType` 基线。
- 其余 default-config 字段仍可正常配置并在 `CreateContext(...)` 中生效。
- default-config surface 的 owner fields 不再制造可见但无效的语义噪音。
