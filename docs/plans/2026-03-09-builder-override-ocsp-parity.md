# Builder Override OCSP Parity (2026-03-09)

## Goal
- 修复 `TSSLContextBuilder.Override(...)` 在 OCSP stapling 布尔字段上的剩余 parity 缺口。
- 让 `ocsp_stapling_enabled` / `ocsp_stapling_required` 通过 `Override(...)` 时，与 `WithOCSPStapling(...)` / `WithOCSPStaplingRequired(...)` 保持同样的状态同步语义。
- 继续保持 config-surface 小切口，不扩展到真实 OCSP runtime 行为。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_ocsp_override_parity.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
1. `Override(...)` 不只是改布尔字段本身，还要与 `FOptions` 联动；否则 `SyncOCSPStaplingOptions` 会被旧 option-set 反向污染。
2. 本波复用已有可观测面：
   - `ExportToJSON`
   - `ExportToINI`
3. 目标语义与 builder methods 保持一致：
   - `ocsp_stapling_required=true` ⇒ `enabled=true`
   - `ocsp_stapling_required=false` 只清 `required`，不主动清 `enabled`
   - `ocsp_stapling_enabled=false` 在没有 `required` 锁住时才会最终关掉 stapling

## RED
1. 新增 focused contract：`tests/config/test_config_ocsp_override_parity.pas`
2. RED 覆盖：
   - `ocsp_stapling_enabled=true`
   - `ocsp_stapling_required=true`
   - `ocsp_stapling_required=true` 后再 `false`
3. RED 结果：
   - override 路径不会把 OCSP 字段写进 JSON/INI
   - 初版修复只改布尔字段仍不够；`required=false` 会被旧 option-set 反向拉回 `true`

## GREEN
1. 修改 `src/fafafa.ssl.context.builder.pas`
   - `Override(...)` 支持：
     - `ocsp_stapling_enabled`
     - `ocsp_stapling_required`
2. 实现与 `WithOCSPStapling*` 对齐：
   - 同步更新 `FOptions`
   - 然后调用 `SyncOCSPStaplingOptions`

## Verification
- `fpc -Fu./src -otmp/test_config_ocsp_override_parity tests/config/test_config_ocsp_override_parity.pas && ./tmp/test_config_ocsp_override_parity`
- `fpc -Fu./src -otmp/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/test_transformation_methods`
- `fpc -Fu./src -otmp/test_config_pkcs11_json_parity tests/config/test_config_pkcs11_json_parity.pas && ./tmp/test_config_pkcs11_json_parity`
- `fpc -Fu./src -otmp/test_config_pkcs11_ini_parity tests/config/test_config_pkcs11_ini_parity.pas && ./tmp/test_config_pkcs11_ini_parity`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- src/fafafa.ssl.context.builder.pas tests/config/test_config_ocsp_override_parity.pas docs/plans/2026-03-09-builder-override-ocsp-parity.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md`

## Notes
- 本波明确沿用了 builder 既有语义，而不是重定义它：`required=false` 不会自动把 `enabled` 一起清掉。
- 下一波可继续看 option-backed builder-only 入口，例如 cert-verify-cache 相关布尔开关是否需要 `Override(...)` 名字级入口。
