# Builder Override PKCS11 JSON Parity (2026-03-09)

## Goal
- 修复 `TSSLContextBuilder.Override(...)` 在 PKCS#11 PIN 配置上的剩余 parity 缺口。
- 让 `pkcs11_uri` / `pkcs11_pin` / `pkcs11_pin_method` 在 `Override(...)`、`ExportToJSON`、`ImportFromJSON`、`Merge(...)` 之间保持一致。
- 保持本波仍是 config-surface 小切口，不扩大到真实 PKCS#11 runtime 行为。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_pkcs11_json_parity.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
1. `Override(...)` 继续作为 builder DSL 的一等入口：
   - `pkcs11_pin` 应与 `WithPKCS11PIN(...)` 一样，自动把 `pkcs11_pin_method` 视为 `value`
   - `pkcs11_pin_method` 使用 case-insensitive 字符串解析
2. JSON 成为本波的可观测面：
   - `pkcs11_uri`
   - `pkcs11_pin`
   - `pkcs11_pin_method`
3. `Merge(...)` 依赖 JSON surface，因此要一起补齐，否则 source builder 仍会静默丢字段。
4. 暂不扩到 INI；该面当前本来就比 JSON 更瘦，单独一波更稳。

## RED
1. 新增 focused contract：`tests/config/test_config_pkcs11_json_parity.pas`
2. RED 覆盖：
   - `Override('pkcs11_pin', ...)` 导出 JSON
   - `Override('pkcs11_pin_method', 'Interactive')` 字符串解析
   - JSON round-trip
   - `Merge(...)` 保留 PKCS#11 字段
3. RED 结果：
   - `pkcs11_uri` / `pkcs11_pin` / `pkcs11_pin_method` 都未出现在 JSON 中
   - `Override('pkcs11_pin_method', ...)` 未生效
   - `Merge(...)` 仍然丢失 PKCS#11 字段

## GREEN
1. 在 `src/fafafa.ssl.context.builder.pas` 增加 PKCS#11 PIN method 的 string helper：
   - `PKCS11PINMethodToText(...)`
   - `TryParsePKCS11PINMethodText(...)`
2. `Override(...)` 补：
   - `pkcs11_pin`
   - `pkcs11_pin_method`
3. `ExportToJSON` / `ImportFromJSON` / `Merge(...)` 同步补：
   - `pkcs11_uri`
   - `pkcs11_pin`
   - `pkcs11_pin_method`

## Verification
- `fpc -Fu./src -otmp/test_config_pkcs11_json_parity tests/config/test_config_pkcs11_json_parity.pas && ./tmp/test_config_pkcs11_json_parity`
- `fpc -Fu./src -otmp/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/test_transformation_methods`
- `fpc -Fu./src -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- src/fafafa.ssl.context.builder.pas tests/config/test_config_pkcs11_json_parity.pas docs/plans/2026-03-09-builder-override-pkcs11-json-parity.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md`

## Notes
- 相邻 `tests/config/test_config_snapshot_clone.pas` 仍有旧的 build-path 失败；该文件未导入任何 backend registration unit，这一类失败早于本波，不在本波扩修范围。
- 下一波最自然的是继续补 PKCS#11 的 INI parity，或者回到剩余 builder-only 字段的 `Override(...)` / import surface 一致性。
