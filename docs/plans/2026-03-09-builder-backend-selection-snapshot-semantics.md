# 2026-03-09 Builder Backend Selection Snapshot Semantics

## Goal
- 收口 backend-selection state（`FAutoSelectBackend` / `FBackendRequirements` / `FExplicitBackend*`）在 builder snapshot / clone surface 上的语义缺口。
- 避免 explicit backend、auto-selection requirements 只存在于“活体 builder 状态”里，经过 `Clone` / `ExportToJSON` / `ImportFromJSON` / `ExportToINI` / `ImportFromINI` / `Merge(...)` / `Reset` 后静默漂移。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_backend_selection_snapshot_semantics.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 backend-selection state 缺口
- [x] 新增 focused RED contract
- [x] 最小修复 clone / JSON / INI / merge / reset surface
- [x] 跑 focused + 相邻回归 + compile-all
- [x] 回写 working memory 与月度汇总

## Verification
- `fpc -Fu./src -otmp/test_config_backend_selection_snapshot_semantics tests/config/test_config_backend_selection_snapshot_semantics.pas && ./tmp/test_config_backend_selection_snapshot_semantics` => PASS (`77/77`)
- `fpc -Fu./src -otmp/test_config_private_key_password_snapshot_semantics tests/config/test_config_private_key_password_snapshot_semantics.pas && ./tmp/test_config_private_key_password_snapshot_semantics` => PASS (`8/8`)
- `fpc -Fu./src -otmp/test_config_merge_string_field_empty_value_semantics tests/config/test_config_merge_string_field_empty_value_semantics.pas && ./tmp/test_config_merge_string_field_empty_value_semantics` => PASS (`6/6`)
- `fpc -Fu./src -otmp/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone` => FAIL (`18/22`) [existing build-path failures in `Test_Reset_Chaining` / `Test_Reset_Rebuild`]
- `fpc -Fu./src -otmp/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export` => FAIL (`46/47`) [existing `No SSL library available` at Test 16]
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)

## Result
- backend-selection state 现在进入 builder 的 `Clone` / `ExportToJSON` / `ImportFromJSON` / `ExportToINI` / `ImportFromINI` / `Merge(...)` / `Reset` surface。
- 这意味着：
  - explicit backend 不再在 clone / round-trip 后静默丢失
  - auto-selection requirements 不再在 JSON / INI / merge 后退化成默认值
  - `Reset` 现在会把 backend-selection state 一起恢复到 constructor 默认值

## Adjacent Audit
- 这波修的是“state 可见性/持久化”而不是“mode 归一化”：`WithAutoBackendSelection` / `Require*` 仍可能让 `FAutoSelectBackend=True` 与 `FExplicitBackendSet=True` 共存，而 build path 目前由 `FAutoSelectBackend` 优先。
- `tests/config/test_config_snapshot_clone.pas` 的 build-path 失败仍是旧问题；它没有引入 backend registration fixture，这与本波 snapshot/parity 修复无关。

## Next Queue
- 把 backend-selection 进一步收口成互斥状态机或显式 precedence contract，避免 auto/explicit 双态共存。
- 单开一波清理 `tests/config/test_config_snapshot_clone.pas` 的 build-path 门禁，让 snapshot/parity 用例不再被环境依赖噪音覆盖。
