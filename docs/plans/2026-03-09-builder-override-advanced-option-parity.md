# 2026-03-09 Builder Override Advanced Option Parity

## Goal
- 收口 `TSSLContextBuilder.Override(...)` 在 `server_name` / `alpn_protocols` / `session_cache_enabled` 上的剩余 option-sync parity 缺口。
- 让 override 路径与 `WithSNI` / `WithALPN` / `WithSessionCache` 保持同样的 `FOptions` 语义。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_advanced_option_override_parity.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 advanced option-coupled 字段缺口
- [x] 新增 focused RED contract
- [x] 最小修改 override 分发
- [x] 跑 focused + 相邻回归 + compile-all
- [x] 回写 working memory 与月度汇总

## Verification
- `fpc -Fu./src -otmp/test_config_advanced_option_override_parity tests/config/test_config_advanced_option_override_parity.pas && ./tmp/test_config_advanced_option_override_parity` => PASS (`10/10`)
- `fpc -Fu./src -otmp/test_config_cert_verify_cache_override_parity tests/config/test_config_cert_verify_cache_override_parity.pas && ./tmp/test_config_cert_verify_cache_override_parity` => PASS (`10/10`)
- `fpc -Fu./src -otmp/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/test_transformation_methods` => PASS (`31/31`)
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)

## Result
- `Override(...)` 现在支持并同步 option set：
  - `server_name`
  - `alpn_protocols`
  - `session_cache_enabled`
- 其中：
  - `server_name` 会同步 `ssoEnableSNI`
  - `alpn_protocols` 会同步 `ssoEnableALPN`
  - `session_cache_enabled` 会同步 `ssoEnableSessionCache`
- focused contract 额外用 `WithoutOption(...)` / `WithSessionCache(False)` 先剥离默认状态，避免默认值把真实 parity 缺口遮住。

## Adjacent Audit
- `ImportFromJSON(...)` / `ImportFromINI(...)` 对这组 advanced option-coupled 字段仍然只是写字段本身；如果外部输入没有自带 `options` 集合，仍可能出现 field-set 与 option-set 漂移。

## Next Queue
- 审 `ImportFromJSON(...)` / `ImportFromINI(...)` 在 `server_name` / `alpn_protocols` / `session_cache_enabled` 上的 option-sync parity。
- 或回到 `TSSLConfig` vs builder DSL 的职责瘦身主线。
