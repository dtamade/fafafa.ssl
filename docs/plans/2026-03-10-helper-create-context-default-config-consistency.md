# 2026-03-10 helper create-context default-config consistency

## Goal
- 让 `tests/helpers/*.inc` 的 fake library 在 `SetDefaultConfig` 后，通过 `CreateContext` 创建出来的 context 真正反映默认配置。
- 修复 helper fixture 只保存快照、但 `CreateContext` 不应用快照的语义缺口。

## Architecture
- 真实 backend 与 `test_factory_shared_config_and_init_race` 内部 fake backend 都满足：`SetDefaultConfig` 后，`CreateContext` 会把默认配置应用到 context。
- 当前 helper fixture 仍然只 `Create` 一个 `TFreePascalContext`，不会应用 `FDefaultConfig`。
- 最小修复是为 helper fixture 提供 baseline snapshot，并在 `CreateContext` 中对复制出的 config 调用 `TSSLFactory.ApplyConfigToContext`。

## Files
- `docs/plans/2026-03-10-helper-create-context-default-config-consistency.md`
- `tests/test_helper_create_context_default_config_consistency.pas`
- `tests/helpers/test_fake_default_backend_fixture.inc`
- `tests/helpers/test_backend_store_fake_fixture.inc`
- `docs/PLANS_CURRENT_INDEX.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. Add a focused RED contract for helper `SetDefaultConfig -> CreateContext` consistency.
2. Verify RED by compiling/running the new Pascal test.
3. Patch helper fixtures with baseline snapshot + `ApplyConfigToContext`.
4. Re-run focused consumer regressions.
5. Update working memory and current summary.

## Expected Verification
- `fpc -Fu./src tests/test_helper_create_context_default_config_consistency.pas -otmp/test_helper_create_context_default_config_consistency`
- `./tmp/test_helper_create_context_default_config_consistency`
- `fpc -Fu./src tests/test_helper_fake_log_callback_snapshot_parity.pas -otmp/test_helper_fake_log_callback_snapshot_parity`
- `./tmp/test_helper_fake_log_callback_snapshot_parity`
- `fpc -Fu./src tests/config/test_config_snapshot_clone.pas -otmp/test_config_snapshot_clone`
- `./tmp/test_config_snapshot_clone`
- `fpc -Fu./src tests/config/test_config_import_export.pas -otmp/test_config_import_export`
- `./tmp/test_config_import_export`
- `fpc -Fu./src tests/test_context_builder_backend_store_consistency.pas -otmp/test_context_builder_backend_store_consistency`
- `./tmp/test_context_builder_backend_store_consistency`
