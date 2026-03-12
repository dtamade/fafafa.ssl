# 2026-03-09 Builder Override Cert Verify Cache Parity

## Goal
- 收口 `TSSLContextBuilder.Override(...)` 在 `cert_verify_cache` / `cert_verify_cache_skip_valid_hit_refresh` 上的剩余 parity 缺口。
- 让 override 路径与 `WithCertVerifyCache*` builder 方法保持相同的 options 语义。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_cert_verify_cache_override_parity.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 cert-verify-cache override 缺口
- [x] 新增 focused RED contract
- [x] 最小修改 override 分发
- [x] 跑 focused + 相邻回归 + compile-all
- [x] 回写 working memory 与月度汇总

## Verification
- `fpc -Fu./src -otmp/test_config_cert_verify_cache_override_parity tests/config/test_config_cert_verify_cache_override_parity.pas && ./tmp/test_config_cert_verify_cache_override_parity` => PASS (`10/10`)
- `fpc -Fu./src -otmp/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/test_transformation_methods` => PASS (`31/31`)
- `fpc -Fu./src -otmp/test_config_ocsp_override_parity tests/config/test_config_ocsp_override_parity.pas && ./tmp/test_config_ocsp_override_parity` => PASS (`12/12`)
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)

## Result
- `Override(...)` 现在支持：
  - `cert_verify_cache`
  - `cert_verify_cache_skip_valid_hit_refresh`
- 这两个字段会直接同步到 `FOptions`，因此 JSON / INI `options` 面与 builder methods 保持一致。
- 本波没有改变任何额外耦合语义；仍保持 `WithCertVerifyCache*` 现有的 option-only 行为。

## Adjacent Audit
- `Override('server_name', ...)` / `Override('alpn_protocols', ...)` / `Override('session_cache_enabled', ...)` 当前仍值得继续审一次 option-sync parity，因为对应 builder methods 也会改 `FOptions`。

## Next Queue
- 审 `server_name` / `alpn_protocols` / `session_cache_enabled` 这类 option-coupled 字段在 `Override(...)` 上的 option-sync parity。
- 或回到 `TSSLConfig` vs builder DSL 的职责瘦身主线。
