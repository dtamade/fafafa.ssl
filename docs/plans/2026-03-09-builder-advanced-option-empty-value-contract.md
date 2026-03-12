# 2026-03-09 Builder Advanced Option Empty Value Contract

## Goal
- 把 `server_name=''` / `alpn_protocols=''` 在 override 与 import 入口上的既有空值语义显式合同化。
- 防止后续 refactor 在无意间把“空值也启用 option”与“field-only import 空值不启用 option”两条语义混在一起。

## Scope
- `tests/config/test_config_advanced_option_empty_value_semantics.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点空值语义现状
- [x] 新增 focused contract
- [x] 合同未暴露真缺口，无需生产修复
- [x] 跑 focused + 相邻回归 + compile-all
- [x] 回写 working memory 与月度汇总

## Verification
- `fpc -Fu./src -otmp/test_config_advanced_option_empty_value_semantics tests/config/test_config_advanced_option_empty_value_semantics.pas && ./tmp/test_config_advanced_option_empty_value_semantics` => PASS (`12/12`)
- `fpc -Fu./src -otmp/test_config_advanced_option_import_parity tests/config/test_config_advanced_option_import_parity.pas && ./tmp/test_config_advanced_option_import_parity` => PASS (`10/10`)
- `fpc -Fu./src -otmp/test_config_advanced_option_override_parity tests/config/test_config_advanced_option_override_parity.pas && ./tmp/test_config_advanced_option_override_parity` => PASS (`10/10`)
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)

## Result
- 空值边界已显式合同化，且现有语义被确认保留：
  - `Override('server_name', '')` 继续匹配 `WithSNI('')`
  - `Override('alpn_protocols', '')` 继续匹配 `WithALPN('')`
  - field-only `ImportFromJSON(...)` / `ImportFromINI(...)` 在空值且未显式提供 `options` 时，不会自动启用 SNI / ALPN option
  - 显式 `options` 仍优先
- 这波是 contract codification wave，不是 bugfix wave；因此无需修改生产代码。

## Adjacent Audit
- `Merge(...)` 目前对 `server_name` / `alpn_protocols` 仍只在非空时赋值，empty-value 更接近“忽略输入”而不是“显式清空”；如果继续 builder 线，这里是下一波最值得合同化的边界。

## Next Queue
- 审 `Merge(...)` 对 `server_name` / `alpn_protocols` / `session_cache_enabled` 的 empty-value / option-sync 语义是否需要显式合同化。
- 或回到 `TSSLConfig` vs builder DSL 的职责瘦身主线。
