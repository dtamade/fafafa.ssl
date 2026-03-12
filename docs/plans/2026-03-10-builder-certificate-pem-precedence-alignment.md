# 2026-03-10 builder certificate PEM precedence alignment

## Goal
- 收口 `TSSLContextBuilder` 在 `certificate_file` 与 `certificate_pem` 同时存在时的 build-vs-validation 语义漂移。
- 让 `BuildClient` / `BuildServer` 与 server validation 文案保持一致：当 file 和 PEM 并存时，PEM 生效。

## Root Cause
- `WithCertificatePEM(...)` / `Override('certificate_pem', ...)` 会清空 `certificate_file`，但 `ImportFromJSON(...)` / `Merge(...)` 可以留下真实的双态。
- 原实现里 `BuildClient` / `BuildServer` 仍先 `LoadCertificate(file)`，再处理 PEM；如果 file 路径无效，会在真正使用 PEM 前直接抛异常。
- focused RED 用 `ImportFromJSON({"certificate_pem": ...})` 叠加到已有 `certificate_file` 上，稳定复现成 `ESSLFileNotFoundException`。

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
- RED：focused suite 先在 imported `file + PEM` 双态下失败，报 missing certificate file。
- GREEN：build 路径改成 `PEM > file` 后，client/server 都能使用 imported PEM 成功构建。
- Verification：focused builder suite、config validation suite、compile-all 全绿。

## Verification
- `fpc -gl -Fu./src -Fu./tests -Fu./tests/helpers -otmp/test_context_builder_backend_store_consistency tests/test_context_builder_backend_store_consistency.pas && ./tmp/test_context_builder_backend_store_consistency` => PASS
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation` => PASS
- `python3 -u scripts/compile_all_modules.py` => PASS (`231/231`)

## Result
- `BuildClient` / `BuildServer` 现在在证书来源上也和 validation 文案一致。
- `ImportFromJSON` / `Merge` 留下的 `certificate_file + certificate_pem` 双态不再误触 file-first 分支。
