# 2026-03-10 builder PKCS11 certificate-required doc truth

## Goal
- 把 `UsePKCS11(...)` 的用户面语义说清楚：它只替代私钥来源，不替代 server 证书要求。
- 修掉 `docs/guides/PKCS11_USER_GUIDE.md` 中已漂移的旧 builder API（`ForServer` / `WithPKCS11Key` / `.Build`）。

## Root Cause
- 行为层面，`ValidateServer` 一直要求 server certificate，哪怕 `pkcs11_uri` 已提供私钥。
- 但用户文档还停在旧 builder API，而且没有明确说明“PKCS11 只管私钥，证书仍需单独提供”。
- 这会让调用方误以为 `UsePKCS11` / 旧 `WithPKCS11Key` 能直接替代整套 server identity。

## Files
- `tests/config/test_config_validation.pas`
- `tests/scripts/test_pkcs11_builder_docs_current_api_contract.sh`
- `README.md`
- `docs/README.md`
- `docs/guides/PKCS11_USER_GUIDE.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Commands
1. `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation`
2. `bash tests/scripts/test_pkcs11_builder_docs_current_api_contract.sh`
3. `fpc -gl -Fu./src -Fu./tests -Fu./tests/helpers -otmp/test_context_builder_backend_store_consistency tests/test_context_builder_backend_store_consistency.pas && ./tmp/test_context_builder_backend_store_consistency`
4. `python3 -u scripts/compile_all_modules.py`
5. `git diff --check -- src/fafafa.ssl.context.builder.pas tests/config/test_config_validation.pas tests/scripts/test_pkcs11_builder_docs_current_api_contract.sh README.md docs/README.md docs/guides/PKCS11_USER_GUIDE.md`

## Expected Outputs
- RED：docs contract 先失败，指出用户指南仍使用旧 API，且未明确“证书仍必需”。
- GREEN：validation suite 显式覆盖 `UsePKCS11` 但无证书仍报 cert error；README / docs README / PKCS11 guide 同步到当前 builder API 与合同。
- Verification：docs contract、validation suite、builder focused suite、compile-all 全绿。

## Verification
- `bash tests/scripts/test_pkcs11_builder_docs_current_api_contract.sh` => PASS
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation` => PASS
- `fpc -gl -Fu./src -Fu./tests -Fu./tests/helpers -otmp/test_context_builder_backend_store_consistency tests/test_context_builder_backend_store_consistency.pas && ./tmp/test_context_builder_backend_store_consistency` => PASS
- `python3 -u scripts/compile_all_modules.py` => PASS (`231/231`)
- `git diff --check -- src/fafafa.ssl.context.builder.pas tests/config/test_config_validation.pas tests/scripts/test_pkcs11_builder_docs_current_api_contract.sh README.md docs/README.md docs/guides/PKCS11_USER_GUIDE.md` => PASS

## Result
- `UsePKCS11(...)` 的行为合同和用户文档现在一致：只替代私钥来源，server 证书仍必需。
- PKCS11 用户指南也已切到当前 builder API：`UsePKCS11(...)` + `BuildServer`。
