# 2026-03-10 fake backend library default config validation parity

## Goal
- 让 `tests/test_factory_shared_config_and_init_race.pas` 里的 fake backend 在 `SetDefaultConfig` 上对齐真实 backend 与 helper fixture 的 library-default validation / owner normalization 语义。
- 修复 fake backend 仍然直接 `FDefaultConfig := AConfig`，会静默接受 request-only / dead field，并允许 owner 字段漂移的缺口。

## Architecture
- helper fixture 已经走 `NormalizeLibraryDefaultOwnerFields -> ValidateLibraryDefaultConfigFields -> NormalizeConfig`。
- shared-config/init-race 主线里的 fake backend 还没收口这条链，因此同一仓库里出现了两个不同的 library-default contract。
- 最小正确修复是在当前 fake backend 的 `SetDefaultConfig` 中直接复用 factory 现有规则，不改 baseline 初始化语义。

## Files
- `docs/plans/2026-03-10-fake-backend-library-default-config-validation-parity.md`
- `tests/test_factory_shared_config_and_init_race.pas`
- `docs/PLANS_CURRENT_INDEX.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. Extend the shared-config/init-race contract with a `SetDefaultConfig` RED.
2. Verify RED by compiling/running the focused Pascal test.
3. Patch the fake backend `SetDefaultConfig` implementation.
4. Re-run focused regressions.
5. Update working memory and current summary.

## Expected Verification
- `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race`
- `./tmp/test_factory_shared_config_and_init_race`
- `fpc -Fu./src tests/test_helper_library_default_config_validation_parity.pas -otmp/test_helper_library_default_config_validation_parity`
- `./tmp/test_helper_library_default_config_validation_parity`
- `fpc -Fu./src tests/test_helper_log_dispatch_parity.pas -otmp/test_helper_log_dispatch_parity`
- `./tmp/test_helper_log_dispatch_parity`
