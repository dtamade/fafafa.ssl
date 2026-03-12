# 2026-03-09 builder implicit default backend resolution consistency

## Goal
- 收口 builder 在 implicit-default 路径上对 backend 的双次 autodetect。
- 保证 context 与 system-roots certificate store 共享同一次 concrete backend 解析，避免中途 default backend 漂移造成不一致。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/test_context_builder_backend_store_consistency.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Root Cause
- `BuildClient` / `BuildServer` 的 implicit-default 路径之前会先 `CreateContext(..., sslAutoDetect)`，随后又在 `WithSystemRoots` 分支对 `CreateCertificateStore(sslAutoDetect)` 发第二次 factory 调用。
- 如果 default backend 在这两个调用之间变化，context 与 store 可能落到不同 backend。
- focused RED 通过一个会在 `CreateContext` 时切换 default backend 的 fake library，把这个理论窗口稳定复现成 client/server store drift。

## Plan
1. 在现有 backend-store consistency suite 中增加 drifting default backend 夹具与 focused RED。
2. 验证当前实现会把 context/store 分别落到不同 backend。
3. 最小修复为 implicit-default 路径先解析一次 concrete backend，再复用。
4. 跑 focused + config 相邻回归 + compile-all。

## Commands
```bash
fpc -gl -Fu./src -otmp/test_context_builder_backend_store_consistency tests/test_context_builder_backend_store_consistency.pas && ./tmp/test_context_builder_backend_store_consistency
fpc -Fu./src -otmp/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone
fpc -Fu./src -otmp/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export
fpc -Fu./src -otmp/test_config_backend_selection_snapshot_semantics tests/config/test_config_backend_selection_snapshot_semantics.pas && ./tmp/test_config_backend_selection_snapshot_semantics
fpc -Fu./src -otmp/test_config_backend_selection_mode_normalization tests/config/test_config_backend_selection_mode_normalization.pas && ./tmp/test_config_backend_selection_mode_normalization
python3 -u scripts/compile_all_modules.py
```

## Expected
- RED 先看到 implicit-default client drift fail
- 修复后 client/server drift contracts 都 PASS
- config focused suites 保持 PASS
- `compile_all_modules.py` PASS
