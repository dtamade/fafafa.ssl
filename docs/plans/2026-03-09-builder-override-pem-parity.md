# Builder Override PEM Parity (2026-03-09)

## Goal
- 修复 `TSSLContextBuilder.Override(...)` 与 builder DSL 其它入口之间的字段覆盖不一致。
- 让 `certificate_pem` / `private_key_pem` 通过 `Override(...)` 也能成为一等字段，而不是只在 `With*PEM` 与 `ImportFromJSON/INI` 可用。
- 保持 builder 的“最后一次设置生效”语义：当 file 与 PEM 同属一个材料位时，新的 override 应清掉互斥字段。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/test_transformation_methods.pas`
- `tests/test_context_builder_backend_store_consistency.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
1. 把 `Override(...)` 视为 builder DSL 的另一种写法，而不是弱化版入口。
2. 最小收口本波只补 PEM 材料字段：
   - `certificate_pem`
   - `private_key_pem`
   - 顺带接受 `private_key_password`
3. 对 file/PEM 成对字段保持互斥：
   - 设置 `certificate_pem` 时清空 `certificate_file`
   - 设置 `private_key_pem` 时清空 `private_key_file`
   - 反向也一样，保持“最后一次设置生效”

## RED
1. 扩展 `tests/test_transformation_methods.pas`
   - 新增 `Test_Override_PEMMaterialFields`
   - 通过 `Override('certificate_pem', LCert)` + `Override('private_key_pem', LKey)` 构造 server builder
   - 断言 `TryBuildServer(...)` 成功
2. RED run:
   - `fpc -Fu./src -otmp/test_transformation_methods_red tests/test_transformation_methods.pas && ./tmp/test_transformation_methods_red`
   - 修复前失败：`Server context requires a certificate`

## GREEN
1. 修改 `src/fafafa.ssl.context.builder.pas`
   - `Override(...)` 支持 `certificate_pem` / `private_key_pem` / `private_key_password`
   - file 与 PEM 的 override 互相清空对方，保持与 `WithCertificate*` / `WithPrivateKey*` 一致的 last-write-wins 语义
2. 保持其余 `Override(...)` 字段行为不变，避免一次性扩太大面

## Verification
- `fpc -Fu./src -otmp/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/test_transformation_methods`
- `fpc -Fu./src -otmp/test_context_builder_backend_store_consistency tests/test_context_builder_backend_store_consistency.pas && ./tmp/test_context_builder_backend_store_consistency`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- src/fafafa.ssl.context.builder.pas tests/test_transformation_methods.pas tests/test_context_builder_backend_store_consistency.pas docs/plans/2026-03-09-builder-override-pem-parity.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md`

## Notes
- 邻近的 `tests/config/test_config_import_export.pas` 仍有旧问题：`Test 16` 在当前环境下没有注册后端，失败于 `No SSL library available`；这不是本波引入。
- 下一波可以继续补 `Override(...)` 对剩余 builder-only 字段的 parity，例如 `use_system_roots`、PKCS#11 相关字段等。
