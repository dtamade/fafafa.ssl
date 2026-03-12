# 2026-03-09 builder server default backend store consistency

## Goal
- 修复 `BuildServer` 在 implicit-default + `WithSystemRoots` 路径上未初始化 `SelectedBackend` 的问题。
- 保证 server build 与 client build 一样，在默认后端路径上把 system roots 加载到正确的 backend-specific store。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/test_context_builder_backend_store_consistency.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Root Cause
- `BuildClient` 在函数入口先做 `SelectedBackend := sslAutoDetect`。
- `BuildServer` 缺少同样初始化，但稍后会在 `FUseSystemRoots=True` 时调用 `TSSLFactory.CreateCertificateStore(SelectedBackend)`。
- 当走 implicit-default 路径时，`SelectedBackend` 可能是未定义栈值，当前已可通过 focused test 复现为 `EAccessViolation`。

## Plan
1. 在现有 backend-store consistency suite 里加 server implicit-default + system-roots focused RED。
2. 用 RED 栈确认崩溃落在 `CreateCertificateStore(SelectedBackend)`。
3. 以最小改动把 `BuildServer` 初始化对齐到 `BuildClient`。
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
- Focused RED 先复现 `EAccessViolation`
- 修复后 `test_context_builder_backend_store_consistency` PASS
- config focused suites 保持 PASS
- `compile_all_modules.py` PASS
