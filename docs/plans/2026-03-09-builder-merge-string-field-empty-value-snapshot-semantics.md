# 2026-03-09 Builder Merge String Field Empty Value Snapshot Semantics

## Goal
- 收口 `TSSLContextBuilder.Merge(...)` 在 certificate / key / CA / PKCS#11 / cipher string fields 上的 source-snapshot empty-value 语义。
- 防止 merge 后 advanced fields 已经按 source snapshot 清空，但其他字符串字段仍残留旧值，形成新的 merge 语义分层。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_merge_string_field_empty_value_semantics.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 `Merge(...)` string-field snapshot 语义
- [x] 新增 focused RED contract
- [x] 最小修复 `Merge(...)` 对显式空字符串的处理
- [x] 跑 focused + 相邻回归 + compile-all
- [x] 回写 working memory 与月度汇总

## Verification
- `fpc -Fu./src -otmp/test_config_merge_string_field_empty_value_semantics tests/config/test_config_merge_string_field_empty_value_semantics.pas && ./tmp/test_config_merge_string_field_empty_value_semantics` => PASS (`6/6`)
- `fpc -Fu./src -otmp/test_config_advanced_option_merge_semantics tests/config/test_config_advanced_option_merge_semantics.pas && ./tmp/test_config_advanced_option_merge_semantics` => PASS (`6/6`)
- `fpc -Fu./src -otmp/test_config_pkcs11_json_parity tests/config/test_config_pkcs11_json_parity.pas && ./tmp/test_config_pkcs11_json_parity` => PASS (`14/14`)
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)

## Result
- `Merge(...)` 现在会在 source snapshot 显式提供空字符串时清空目标字符串字段，而不再把“空值”误当成“未指定”。
- 这波覆盖的字段包括：
  - `certificate_file` / `certificate_pem`
  - `private_key_file` / `private_key_pem`
  - `ca_file` / `ca_path`
  - `pkcs11_uri` / `pkcs11_pin`
  - `cipher_list` / `tls13_ciphersuites`
- `pkcs11_pin_method` 的 default reset 语义继续通过既有解析路径保留。

## Adjacent Audit
- `FPrivateKeyPassword` 目前仍不进入 `ExportToJSON` / `ImportFromJSON` / `Merge(...)`；merge key file / PEM 时会带路径但不带密码。
- backend-selection 状态（`FAutoSelectBackend` / `FBackendRequirements` / `FExplicitBackend*`）也尚未进入 snapshot surface。

## Next Queue
- 审 `Merge(...)` / snapshot surface 对 `FPrivateKeyPassword` 与 backend-selection 状态的缺口。
- 或回到 `TSSLConfig` vs builder DSL 的职责瘦身主线。
