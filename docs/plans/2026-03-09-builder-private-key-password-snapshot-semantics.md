# 2026-03-09 Builder Private Key Password Snapshot Semantics

## Goal
- 收口 `private_key_password` 在 `ExportToJSON` / `ImportFromJSON` / `ExportToINI` / `ImportFromINI` / `Merge(...)` 上的 snapshot 语义。
- 避免 builder snapshot 保留了私钥路径 / PEM，却静默丢失与之配套的 password。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_private_key_password_snapshot_semantics.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 password snapshot 缺口
- [x] 新增 focused RED contract
- [x] 最小修复 JSON/INI/import/merge surface
- [x] 跑 focused + 相邻回归 + compile-all
- [x] 回写 working memory 与月度汇总

## Verification
- `fpc -Fu./src -otmp/test_config_private_key_password_snapshot_semantics tests/config/test_config_private_key_password_snapshot_semantics.pas && ./tmp/test_config_private_key_password_snapshot_semantics` => PASS (`8/8`)
- `fpc -Fu./src -otmp/test_config_merge_string_field_empty_value_semantics tests/config/test_config_merge_string_field_empty_value_semantics.pas && ./tmp/test_config_merge_string_field_empty_value_semantics` => PASS (`6/6`)
- `fpc -Fu./src -otmp/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export` => FAIL (`46/47`) [existing `No SSL library available` at Test 16]
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)

## Result
- `private_key_password` 现在进入 builder 的 JSON / INI snapshot surface，并参与 `ImportFromJSON(...)` / `ImportFromINI(...)` / `Merge(...)`。
- 这意味着：
  - JSON / INI round-trip 不再丢失私钥密码
  - `Merge(...)` 可以用 source password 覆盖目标 password
  - source 的空 password 也可以显式清空目标 password

## Adjacent Audit
- backend-selection 状态（`FAutoSelectBackend` / `FBackendRequirements` / `FExplicitBackend*`）仍然不在 `Clone` / `ExportToJSON` / `ImportFromJSON` / `Merge(...)` surface 上，是当前最清晰的下一层缺口。

## Next Queue
- 审 backend-selection 状态在 `Clone` + snapshot surface 上的缺口。
- 或单独整理 `tests/config/test_config_import_export.pas` 那条既有环境依赖失败的门禁说明。
