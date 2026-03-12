# 2026-03-09 Builder Import Advanced Option Parity

## Goal
- 收口 `TSSLContextBuilder.ImportFromJSON(...)` / `ImportFromINI(...)` 在 `server_name` / `alpn_protocols` / `session_cache_enabled` 上的剩余 option-sync parity 缺口。
- 让 field-only 输入在缺失 `options` 面时，仍与 `WithSNI` / `WithALPN` / `WithSessionCache` 保持一致。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_advanced_option_import_parity.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 advanced import parity 缺口
- [x] 新增 focused RED contract
- [x] 最小修改 JSON/INI import 分发
- [x] 跑 focused + 相邻回归 + compile-all
- [x] 回写 working memory 与月度汇总

## Verification
- `fpc -Fu./src -otmp/test_config_advanced_option_import_parity tests/config/test_config_advanced_option_import_parity.pas && ./tmp/test_config_advanced_option_import_parity` => PASS (`10/10`)
- `fpc -Fu./src -otmp/test_config_advanced_option_override_parity tests/config/test_config_advanced_option_override_parity.pas && ./tmp/test_config_advanced_option_override_parity` => PASS (`10/10`)
- `fpc -Fu./src -otmp/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/test_transformation_methods` => PASS (`31/31`)
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)

## Result
- `ImportFromJSON(...)` / `ImportFromINI(...)` 在缺失显式 `options` 时，现在会为 advanced option-coupled fields 做最小同步：
  - `server_name` -> `ssoEnableSNI`
  - `alpn_protocols` -> `ssoEnableALPN`
  - `session_cache_enabled` -> `ssoEnableSessionCache`
- 若输入显式带了 `options` 面，本波实现不会强行覆盖那组显式 options。
- 在此基础上，本波还把 advanced option sync 抽成一个 shared helper，供 `Override(...)`、`ImportFromJSON(...)`、`ImportFromINI(...)` 复用，降低后续再漂移概率。

## Adjacent Audit
- advanced option sync 的行为现在已经集中，但 `server_name=''` / `alpn_protocols=''` 这种空字符串边界仍带有入口差异：override 仍会启用对应 option，而 field-only import 只在非空时启用；这属于既有语义，不在本波重定义。

## Next Queue
- 若继续 builder 侧治理，下一波可专门把 `server_name=''` / `alpn_protocols=''` 这类空值语义显式合同化，避免未来被无意改掉。
- 或回到 `TSSLConfig` vs builder DSL 的职责瘦身主线。
