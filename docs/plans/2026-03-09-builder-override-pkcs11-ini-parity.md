# Builder Override PKCS11 INI Parity (2026-03-09)

## Goal
- 修复 PKCS#11 字段在 `ExportToINI` / `ImportFromINI` 上的剩余 parity 缺口。
- 让 `pkcs11_uri` / `pkcs11_pin` / `pkcs11_pin_method` 在 INI 面与 JSON 面保持一致。
- 保持本波继续是 config-surface 小切口，不扩展到真实 PKCS#11 runtime。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_pkcs11_ini_parity.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
1. INI 现在补上 PKCS#11 明确分区：`[PKCS11]`。
2. 继续复用上一波已经引入的 PIN method string helper：
   - `PKCS11PINMethodToText(...)`
   - `TryParsePKCS11PINMethodText(...)`
3. 本波只补 INI：
   - `ExportToINI`
   - `ImportFromINI`
4. 不触碰更大的 import/export 总套件；用 focused contract 避免被旧失败面干扰。

## RED
1. 新增 focused contract：`tests/config/test_config_pkcs11_ini_parity.pas`
2. RED 覆盖：
   - INI export
   - INI import
   - INI round-trip
3. 修复前观察到：
   - `pkcs11_uri` / `pkcs11_pin` / `pkcs11_pin_method` 都未出现在 INI 中
   - `ImportFromINI` 也会静默丢弃这三个字段

## GREEN
1. 在 `ExportToINI` 中新增 `[PKCS11]` section：
   - `pkcs11_uri`
   - `pkcs11_pin`
   - `pkcs11_pin_method`
2. 在 `ImportFromINI` 中解析：
   - `pkcs11_uri`
   - `pkcs11_pin`
   - `pkcs11_pin_method`
3. 继续沿用大小写不敏感的 PIN method 文本解析。

## Verification
- `fpc -Fu./src -otmp/test_config_pkcs11_ini_parity tests/config/test_config_pkcs11_ini_parity.pas && ./tmp/test_config_pkcs11_ini_parity`
- `fpc -Fu./src -otmp/test_config_pkcs11_json_parity tests/config/test_config_pkcs11_json_parity.pas && ./tmp/test_config_pkcs11_json_parity`
- `fpc -Fu./src -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- src/fafafa.ssl.context.builder.pas tests/config/test_config_pkcs11_ini_parity.pas docs/plans/2026-03-09-builder-override-pkcs11-ini-parity.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md`

## Notes
- 下一波最自然的是继续收口 `Override(...)` 对 OCSP stapling 布尔字段的 parity，因为 JSON/INI 已有、但 override 入口还没补齐。
