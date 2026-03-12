# 2026-03-10 fake backend log dispatch parity

## Goal
- 让 `tests/test_factory_shared_config_and_init_race.pas` 里的 fake backend 在 `Log(...)` 上对齐真实 backend 与 helper fixture 的 callback dispatch 语义。
- 修复 fake backend 虽然已经保存 `LogLevel` / `LogCallback`，但 `Log(...)` 仍是 no-op 的缺口。

## Architecture
- 真实 backend 与 helper fixture 现在都满足：`Log(...)` 在 `Assigned(LogCallback)` 且 `ALevel <= LogLevel` 时分发 callback。
- `test_factory_shared_config_and_init_race.pas` 内部 fake backend 仍只覆盖 snapshot/setter，不覆盖 runtime dispatch。
- 最小修复是在当前 fake backend 的 `Log(...)` 中直接复用 `FDefaultConfig.LogCallback` / `FDefaultConfig.LogLevel`。

## Files
- `docs/plans/2026-03-10-fake-backend-log-dispatch-parity.md`
- `tests/test_factory_shared_config_and_init_race.pas`
- `docs/PLANS_CURRENT_INDEX.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. Extend the shared-config/init-race contract with a logging dispatch RED.
2. Verify RED by compiling/running the focused Pascal test.
3. Patch the fake backend `Log(...)` implementation.
4. Re-run focused regressions.
5. Update working memory and current summary.

## Expected Verification
- `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race`
- `./tmp/test_factory_shared_config_and_init_race`
- `fpc -Fu./src tests/test_helper_log_dispatch_parity.pas -otmp/test_helper_log_dispatch_parity`
- `./tmp/test_helper_log_dispatch_parity`
