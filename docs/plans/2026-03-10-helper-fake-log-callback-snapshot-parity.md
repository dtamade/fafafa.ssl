# 2026-03-10 helper fake log callback snapshot parity

## Goal
- 让 `tests/helpers/test_fake_default_backend_fixture.inc` 与 `tests/helpers/test_backend_store_fake_fixture.inc` 的 fake library 在 `SetLogCallback` 上与真实 backend 保持同样的 snapshot 可见性。
- 避免 helper 夹具继续把 `SetLogCallback` 做成 no-op，导致消费这些夹具的测试无法承接 logging-scope 合同。

## Architecture
- 真实 backend 与 `test_factory_shared_config_and_init_race` 内部 fake backend 已经收口到：`SetLogCallback` 会同步 `GetDefaultConfig.LogCallback`。
- 两个 `tests/helpers/*.inc` 夹具仍然没有保存默认配置，也不会把 `SetLogCallback` 写回默认快照。
- 最小修复是在 helper fake library 内保存 `FDefaultConfig`，并让 `SetDefaultConfig` / `GetDefaultConfig` / `SetLogCallback` 对齐到同一份快照。

## Files
- `docs/plans/2026-03-10-helper-fake-log-callback-snapshot-parity.md`
- `tests/test_helper_fake_log_callback_snapshot_parity.pas`
- `tests/helpers/test_fake_default_backend_fixture.inc`
- `tests/helpers/test_backend_store_fake_fixture.inc`
- `docs/PLANS_CURRENT_INDEX.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. Add a focused RED contract covering both helper fixtures.
2. Verify RED by compiling/running the new Pascal test.
3. Patch helper fake libraries to keep a default-config snapshot.
4. Re-run focused consumer regressions.
5. Update working memory and current summary.

## Expected Verification
- `fpc -Fu./src tests/test_helper_fake_log_callback_snapshot_parity.pas -otmp/test_helper_fake_log_callback_snapshot_parity`
- `./tmp/test_helper_fake_log_callback_snapshot_parity`
- `fpc -Fu./src tests/config/test_config_snapshot_clone.pas -otmp/test_config_snapshot_clone`
- `./tmp/test_config_snapshot_clone`
- `fpc -Fu./src tests/config/test_config_import_export.pas -otmp/test_config_import_export`
- `./tmp/test_config_import_export`
- `fpc -Fu./src tests/test_context_builder_backend_store_consistency.pas -otmp/test_context_builder_backend_store_consistency`
- `./tmp/test_context_builder_backend_store_consistency`
