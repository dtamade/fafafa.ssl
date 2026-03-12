# 2026-03-09 config build-path fake backend fixture

## Goal
- 收口 `tests/config` 中依赖真实默认 SSL 后端的旧失败。
- 让 snapshot/import-export 套件在没有预注册库的环境里也能稳定验证 builder 的 snapshot/build 语义。

## Scope
- `tests/config/test_fake_default_backend_fixture.inc`
- `tests/config/test_config_snapshot_clone.pas`
- `tests/config/test_config_import_export.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `docs/plans/2026-03-current-summary.md`

## Root Cause
- `TryBuildServer` 默认走 `TSSLFactory.CreateContext(..., sslAutoDetect)`。
- 两个测试程序都没有在进程内预注册任何可用库，导致 factory 抛出 `No SSL library available. Please register a library first.`。
- 这属于测试环境依赖噪音，不是 builder snapshot/import/export 语义失败。

## Plan
1. 复现 `test_config_snapshot_clone` 与 `test_config_import_export` 旧失败。
2. 提炼一个本地 fake default backend 夹具，放到 `tests/config` 同目录 include。
3. 只在需要 build-path 的测试块内注册/清理 fake backend，避免污染纯 snapshot/JSON/INI 测试。
4. 跑 focused 回归与 compile-all，确认 builder 相邻波次仍绿。

## Commands
```bash
fpc -Fu./src -otmp/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone
fpc -Fu./src -otmp/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export
fpc -Fu./src -otmp/test_config_backend_selection_snapshot_semantics tests/config/test_config_backend_selection_snapshot_semantics.pas && ./tmp/test_config_backend_selection_snapshot_semantics
fpc -Fu./src -otmp/test_config_backend_selection_mode_normalization tests/config/test_config_backend_selection_mode_normalization.pas && ./tmp/test_config_backend_selection_mode_normalization
python3 -u scripts/compile_all_modules.py
```

## Expected
- `test_config_snapshot_clone` PASS `22/22`
- `test_config_import_export` PASS `47/47`
- 相邻 backend-selection focused tests 保持 PASS
- `compile_all_modules.py` PASS
