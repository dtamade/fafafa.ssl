# 2026-03-10 builder PKCS11 mixed key warning alignment

## Goal
- 收口 `TSSLContextBuilder.ValidateServer` 对 `pkcs11_uri` 与其它私钥来源并存时的可观测语义。
- 让 validation 明确告诉调用方：当 `PKCS#11 URI` 与 file/PEM 私钥同时存在时，build 会优先使用 PKCS#11。

## Root Cause
- build 路径已经是 `PKCS#11 > PEM > file`。
- 但 validation 之前只负责“缺不缺私钥来源”，没有在 `pkcs11_uri + private_key_file/private_key_pem` 并存时发 warning。
- 这会让 mixed-input 状态在 validation 面上看起来“正常且无歧义”，但 build 真行为其实已经有明确 precedence。

## Files
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_validation.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Commands
1. `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation`
2. `fpc -gl -Fu./src -Fu./tests -Fu./tests/helpers -otmp/test_context_builder_backend_store_consistency tests/test_context_builder_backend_store_consistency.pas && ./tmp/test_context_builder_backend_store_consistency`
3. `python3 -u scripts/compile_all_modules.py`

## Expected Outputs
- RED：validation suite 先失败，因为 `pkcs11_uri + file/PEM` 双态没有 precedence warning。
- GREEN：`ValidateServer` 会在 mixed-input 状态下显式提示 `PKCS#11 will be used`。
- Verification：validation suite、backend-store suite、compile-all 全绿。

## Verification
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation` => PASS
- `fpc -gl -Fu./src -Fu./tests -Fu./tests/helpers -otmp/test_context_builder_backend_store_consistency tests/test_context_builder_backend_store_consistency.pas && ./tmp/test_context_builder_backend_store_consistency` => PASS
- `python3 -u scripts/compile_all_modules.py` => PASS (`231/231`)

## Result
- server validation 现在不再把 PKCS11 mixed-input 当作无歧义配置静默放过。
- mixed-input precedence 在 validation 面和 build 面已经对齐：`PKCS#11` 优先。
