# Context Builder ServerName Compatibility Marker

## Goal

执行 `context-level ServerName` 迁移路线图中 Phase B 的第一刀：在不改变当前 runtime compatibility 的前提下，缩窄 builder 的 import/export surface，让 `server_name` 明确表现为 deprecated context-level SNI compatibility，而不是继续像普通推荐字段。

## Architecture

- 只动 builder 持久化 surface：
  - `src/fafafa.ssl.context.builder.pas`
- 不碰 backend constructor fallback：
  - OpenSSL / FreePascal / WinSSL / MbedTLS / WolfSSL 当前兼容语义不变
- 用 focused config tests 锁住 additive compatibility 行为：
  - 新测试直接检查 JSON/INI export/import marker
  - 邻接回归覆盖 import-export、merge、clone/reset

## Files

- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_context_builder_server_name_compat_marker.pas`
- `tests/config/test_config_import_export.pas`
- `tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas`
- `tests/config/test_config_snapshot_clone.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused RED：
   - builder JSON export with `WithSNI(...)` 必须保留 `server_name`
   - JSON export 同时必须带 `server_name_mode=deprecated_context_sni`
   - INI export 同样要求带该 marker
   - legacy JSON/INI bare `server_name` 输入必须继续可导入，并在 re-export 时自动补 marker
2. 生产修复：
   - `ExportToJSON(...)` / `ExportToINI(...)` 在 `server_name <> ''` 时导出兼容 marker
   - `ImportFromJSON(...)` / `ImportFromINI(...)` 显式接受 marker，但不让其改变 runtime state
3. focused regressions：
   - `tests/config/test_context_builder_server_name_compat_marker.pas`
   - `tests/config/test_config_import_export.pas`
   - `tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas`
   - `tests/config/test_config_snapshot_clone.pas`
4. 收口：
   - 更新 `task_plan.md` / `findings.md` / `progress.md`
   - 更新接口/后端验证报告

## Expected Outputs

- builder export surface 对 `server_name` 带出显式兼容 marker
- legacy config import compatibility 保持不变
- focused config regressions 继续绿色
- 下次继续时，可以直接进入 Phase B 第二刀 `factory/config surface narrowing`
