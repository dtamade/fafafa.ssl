# Builder Override Validation Parity (2026-03-09)

## Goal
- 修复 `TSSLContextBuilder.Override(...)` 与 validation DSL 入口的剩余语义漂移。
- 让 `use_system_roots` / `pkcs11_uri` 通过 `Override(...)` 时，和 `WithSystemRoots` / `UsePKCS11` 保持相同的 `ValidateServer` 结果。
- 继续坚持“小切口 parity 收口”，避免一次性扩大到完整 `TSSLConfig` / builder 架构重整。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_validation.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
1. 把 `Override(...)` 继续视为 builder DSL 的一等入口，而不是弱化版快捷写法。
2. 本波只补 validation 可观测缺口：
   - `use_system_roots`
   - `pkcs11_uri`
3. 修复点保持在 `Override(...)` 分发层：
   - `use_system_roots=true` 应设置 `FUseSystemRoots`
   - `pkcs11_uri=...` 应设置 `FPKCS11URI`
4. 先不扩大到 `pkcs11_pin` / `pkcs11_pin_method` 的 export/import 语义；那一层需要额外可观测面，单独开下一波更稳。

## RED
1. 在 `tests/config/test_config_validation.pas` 新增 focused contracts：
   - `Test_ServerPKCS11Validation_ViaOverride`
   - `Test_ServerSystemRootsValidation_ViaOverride`
2. RED 命令：
   - `fpc -Fu./src -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation`
3. 修复前观察到：
   - `Override('pkcs11_uri', ...)` 仍报 server missing private key
   - `Override('use_system_roots', 'true')` 仍保留 `no CA certificates configured` warning

## GREEN
1. 修改 `src/fafafa.ssl.context.builder.pas`
   - `Override(...)` 新增 `use_system_roots`
   - `Override(...)` 新增 `pkcs11_uri`
2. 保持其余 builder / export 语义不变，不把本波扩大成新的配置面重构。

## Verification
- `fpc -Fu./src -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation`
- `fpc -Fu./src -otmp/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/test_transformation_methods`
- `fpc -Fu./src -otmp/test_context_builder_backend_store_consistency tests/test_context_builder_backend_store_consistency.pas && ./tmp/test_context_builder_backend_store_consistency`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- src/fafafa.ssl.context.builder.pas tests/config/test_config_validation.pas docs/plans/2026-03-09-builder-override-validation-parity.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md`

## Notes
- 这波是 validation parity，不是 runtime PKCS#11 capability 扩展；未引入真实 token/runtime 依赖。
- 下一波最自然的是继续补 `pkcs11_pin` / `pkcs11_pin_method`，并通过 `ExportToJSON` / `ImportFromJSON` 或 dedicated contract 把它们变成可观测行为。
