# 2026-03-10 helper log dispatch parity

## Goal
- 让 `tests/helpers/*.inc` 的 fake library 在 `Log(...)` 上对齐真实 backend 的 callback dispatch 语义。
- 修复 helper fixture 虽然保存了 `LogLevel` / `LogCallback`，但 `Log(...)` 仍然是 no-op 的缺口。

## Architecture
- 真实 backend 的 `Log(...)` 最终都会在 `Assigned(LogCallback)` 且 `ALevel <= LogLevel` 时调用 callback。
- 当前 helper fixture 已经有 default snapshot、`SetDefaultConfig` 和 `SetLogCallback`，但 `Log(...)` 仍不做任何事。
- 最小修复是在 helper fixture 的 `Log(...)` 中复用 snapshot 上的 `LogLevel` / `LogCallback`，不引入额外状态。

## Files
- `docs/plans/2026-03-10-helper-log-dispatch-parity.md`
- `tests/test_helper_log_dispatch_parity.pas`
- `tests/helpers/test_fake_default_backend_fixture.inc`
- `tests/helpers/test_backend_store_fake_fixture.inc`
- `docs/PLANS_CURRENT_INDEX.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. Add a focused RED contract for helper `Log(...)` dispatch.
2. Verify RED by compiling/running the new Pascal test.
3. Patch helper fixtures to dispatch callback with level gating.
4. Re-run focused helper regressions.
5. Update working memory and current summary.

## Expected Verification
- `fpc -Fu./src tests/test_helper_log_dispatch_parity.pas -otmp/test_helper_log_dispatch_parity`
- `./tmp/test_helper_log_dispatch_parity`
- `fpc -Fu./src tests/test_helper_library_default_config_validation_parity.pas -otmp/test_helper_library_default_config_validation_parity`
- `./tmp/test_helper_library_default_config_validation_parity`
- `fpc -Fu./src tests/test_helper_create_context_default_config_consistency.pas -otmp/test_helper_create_context_default_config_consistency`
- `./tmp/test_helper_create_context_default_config_consistency`
- `fpc -Fu./src tests/test_helper_fake_log_callback_snapshot_parity.pas -otmp/test_helper_fake_log_callback_snapshot_parity`
- `./tmp/test_helper_fake_log_callback_snapshot_parity`
