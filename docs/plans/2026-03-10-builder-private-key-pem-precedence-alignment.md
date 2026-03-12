# 2026-03-10 builder private-key PEM precedence alignment

## Goal
- 收口 `TSSLContextBuilder` 在 `private_key_file` 与 `private_key_pem` 同时存在时的 build-vs-validation 语义漂移。
- 让 `BuildClient` / `BuildServer` 与 `ValidateServer` 保持一致的私钥来源优先级：`PKCS#11 > PEM > file`。

## Root Cause
- `ValidateServer` 已明确约定：当 `private_key_file` 与 `private_key_pem` 同时存在时，PEM 生效。
- 但 `BuildClient` / `BuildServer` 仍是 `PKCS#11 > file > PEM`，因此在 `ImportFromJSON(...)` / `Merge(...)` 这类可并存状态下，会先尝试加载 file。
- focused RED 用 `ImportFromJSON({"private_key_pem": ...})` 在已有 `private_key_file` 的 builder 上构造出真实双态，稳定复现成 `ESSLFileNotFoundException`。

## Files
- `src/fafafa.ssl.context.builder.pas`
- `tests/test_context_builder_backend_store_consistency.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Commands
1. `fpc -gl -Fu./src -Fu./tests -Fu./tests/helpers -otmp/test_context_builder_backend_store_consistency tests/test_context_builder_backend_store_consistency.pas && ./tmp/test_context_builder_backend_store_consistency`
2. `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation`
3. `python3 -u scripts/compile_all_modules.py`

## Expected Outputs
- RED：focused suite 先在 merged/imported `file + PEM` 状态下失败，报 missing private-key file。
- GREEN：build 路径改成 `PKCS#11 > PEM > file` 后，client/server 两条分支都以 PEM 成功构建。
- Verification：focused builder suite、config validation suite、compile-all 全绿。

## Verification
- `fpc -gl -Fu./src -Fu./tests -Fu./tests/helpers -otmp/test_context_builder_backend_store_consistency tests/test_context_builder_backend_store_consistency.pas && ./tmp/test_context_builder_backend_store_consistency` => PASS
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation` => PASS
- `python3 -u scripts/compile_all_modules.py` => PASS (`231/231`)

## Result
- `BuildClient` / `BuildServer` 现在在私钥来源上和 validation 合同一致。
- `ImportFromJSON` / `Merge` 产生的 `file + PEM` 双态不再误触 file-first 分支。
