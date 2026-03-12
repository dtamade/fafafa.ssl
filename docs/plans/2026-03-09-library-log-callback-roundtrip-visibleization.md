# 2026-03-09 library log callback roundtrip visibleization

## Goal
- 让 `ISSLLibrary.SetLogCallback` 与 `GetDefaultConfig` 的 logging surface 保持一致。
- 修复 library 级 logging 配置经 `SetLogCallback` 修改后，`GetDefaultConfig.LogCallback` 仍回显旧值/空值的可见性缺口。

## Architecture
- `TSSLConfig.LogCallback` 已被工厂明确标记为 library-scoped；request path 会拒绝它。
- 各 backend 当前只在 `SetDefaultConfig` 时同步 `FDefaultConfig.LogCallback`，但 `SetLogCallback` 只写 `FLogCallback`。
- 最小修复是在 backend `SetLogCallback` 中同步 library default snapshot，保证 round-trip surface 一致，而不改变 context 创建语义。

## Files
- `tests/test_library_log_callback_roundtrip_visibleization.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.openssl.backed.pas`
- `src/fafafa.ssl.winssl.lib.pas`
- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `docs/PLANS_CURRENT_INDEX.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. Add focused RED test for `SetLogCallback` -> `GetDefaultConfig` round-trip.
2. Verify RED by compiling/running the new Pascal test.
3. Patch backend `SetLogCallback` to sync default snapshot.
4. Re-run focused regression tests.
5. Update working memory and current summary.

## Expected Verification
- `fpc -Fu./src tests/test_library_log_callback_roundtrip_visibleization.pas`
- `./tests/test_library_log_callback_roundtrip_visibleization`
- `fpc -Fu./src tests/test_factory_request_config_logging_scope_visibleization.pas`
- `./tests/test_factory_request_config_logging_scope_visibleization`
- `fpc -Fu./src tests/config/test_default_config.pas`
- `./tests/config/test_default_config`
