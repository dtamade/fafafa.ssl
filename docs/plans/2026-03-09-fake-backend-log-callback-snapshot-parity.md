# 2026-03-09 fake backend log callback snapshot parity

## Goal
- 让 `tests/test_factory_shared_config_and_init_race.pas` 里的 fake backend 与真实后端保持同样的 logging snapshot 语义。
- 避免测试替身在 `SetLogCallback` 上继续偏离生产实现，削弱后续 logging-scope 合同的可信度。

## Architecture
- 真实后端现在都保证：`SetLogCallback` 会同步 `GetDefaultConfig.LogCallback` 的可见快照。
- `test_factory_shared_config_and_init_race.pas` 内部 fake backend 仍然把 `SetLogCallback` 做成 no-op。
- 最小修复是在同一合同里先补 RED，再让 fake backend 同步 `FDefaultConfig.LogCallback`。

## Files
- `docs/plans/2026-03-09-fake-backend-log-callback-snapshot-parity.md`
- `tests/test_factory_shared_config_and_init_race.pas`
- `docs/PLANS_CURRENT_INDEX.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. Extend the shared-config/init-race contract with a logging snapshot RED.
2. Verify RED by compiling/running the focused Pascal test.
3. Patch the fake backend `SetLogCallback` implementation.
4. Re-run focused regression tests.
5. Update working memory and current summary.

## Expected Verification
- `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race`
- `./tmp/test_factory_shared_config_and_init_race`
- `fpc -Fu./src tests/test_library_log_callback_roundtrip_visibleization.pas -otmp/test_library_log_callback_roundtrip_visibleization`
- `./tmp/test_library_log_callback_roundtrip_visibleization`
