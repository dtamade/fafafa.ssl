# 2026-03-09 Builder Backend Selection Mode Normalization

## Goal
- 把 builder 的 backend-selection 从“字段可持久化”继续收口到“模式可归一化”。
- 避免 `WithAutoBackendSelection` / `Require*` / `WithBackend` 之间留下 inactive stale state，导致 snapshot 虽可 round-trip，却仍带着双态噪音。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_backend_selection_mode_normalization.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 auto/explicit 双态残留
- [x] 新增 focused RED normalization contract
- [x] 最小修复 mode invariants + import normalization
- [x] 跑 focused + 相邻回归 + compile-all
- [x] 回写 working memory 与月度汇总

## Verification
- `fpc -Fu./src -otmp/test_config_backend_selection_mode_normalization tests/config/test_config_backend_selection_mode_normalization.pas && ./tmp/test_config_backend_selection_mode_normalization` => PASS (`32/32`)
- `fpc -Fu./src -otmp/test_config_backend_selection_snapshot_semantics tests/config/test_config_backend_selection_snapshot_semantics.pas && ./tmp/test_config_backend_selection_snapshot_semantics` => PASS (`77/77`)
- `fpc -Fu./src -otmp/test_config_private_key_password_snapshot_semantics tests/config/test_config_private_key_password_snapshot_semantics.pas && ./tmp/test_config_private_key_password_snapshot_semantics` => PASS (`8/8`)
- `fpc -Fu./src -otmp/test_config_merge_string_field_empty_value_semantics tests/config/test_config_merge_string_field_empty_value_semantics.pas && ./tmp/test_config_merge_string_field_empty_value_semantics` => PASS (`6/6`)
- `python3 -u scripts/compile_all_modules.py` => PASS (`231/231`)

## Result
- backend-selection mode 现在按稳定 precedence 归一化：
  - `backend_auto_select=true` 时，清空 inert explicit backend state
  - `backend_explicit_library_set=true` 且 `backend_auto_select=false` 时，清空 stale requirements
  - 只有 requirement fields 出现时，会自动归一为 auto-selection mode
- `WithAutoBackendSelection(...)` / `WithBackend(...)` / `RequireTLS13` / `RequireCipher(...)` / `RequirePKCS11Support` / `PreferOSNative` 现在都不会再留下 inactive stale mode state。
- `ImportFromJSON(...)` / `ImportFromINI(...)` / `Merge(...)` 现在也会把 backend-selection state 归一化后再保留到 builder 中。

## Adjacent Audit
- 这波解决的是 builder 内部 mode noise，不是 build-path 环境噪音；`tests/config/test_config_snapshot_clone.pas` 的 4 个 build-path 失败仍需单独处理。
- 当前 backend-selection state machine 已经比之前清晰很多，但 precedence 仍是隐式代码规则；如果后续要对外公开配置文档，最好把这条 precedence 合同写进 docs/API 注释。

## Next Queue
- 单开 `tests/config/test_config_snapshot_clone.pas` build-path fixture 波次，给 snapshot / clone suite 补稳定 backend registration 夹具。
- 然后再看 `tests/config/test_config_import_export.pas` 的既有 Test 16 环境依赖门禁说明是否要显式化。
