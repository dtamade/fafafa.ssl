# Builder ServerName Compatibility Warning

## Goal

执行 `context-level ServerName` 迁移主线在 builder surface 上的下一刀：让 `TSSLContextBuilder.WithSNI(...)` 在真实 `BuildClient` / `BuildServer` 路径里也显式暴露 deprecated compatibility 语义，而不再只在 validation 里给 warning。

## Architecture

- 只补 builder runtime warning，不改当前兼容行为：
  - `src/fafafa.ssl.context.builder.pas`
- 保持现状：
  - `WithSNI(...)` 仍然可用
  - `BuildClient` 仍会把 `FServerName` 写到 context
  - `BuildServer` 仍会保留 context state，但 server-side connections 继续忽略它
- focused 验证：
  - 新增 builder warning test
  - 邻接回归覆盖 validation 与现有 runtime consistency

## Files

- `src/fafafa.ssl.context.builder.pas`
- `docs/reference/API_REFERENCE.md`
- `tests/test_context_builder_server_name_compatibility_warning.pas`
- `tests/config/test_config_validation.pas`
- `tests/test_context_builder_server_servername_runtime_consistency.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused RED：
   - `BuildClient` with `WithSNI(...)` 必须发 runtime compatibility warning
   - `BuildServer` with `WithSNI(...)` 必须发 runtime warning，且明确 server-side connections ignore it
   - 未设置 `WithSNI(...)` 时保持安静
2. 生产修复：
   - builder 新增 runtime warning helper
   - `BuildClient` / `BuildServer` 在兼容写入前发 warning
   - public note/comment 补充 `WithSNI(...)` 属于 compatibility-only
3. focused regressions：
   - `tests/test_context_builder_server_name_compatibility_warning.pas`
   - `tests/config/test_config_validation.pas`
   - `tests/test_context_builder_server_servername_runtime_consistency.pas`
4. 收口：
   - 更新计划/发现/进度文件
   - 更新 SNI migration 路线图与综合验证报告

## Expected Outputs

- builder runtime path 不再静默应用 `WithSNI(...)`
- validation warning 与 runtime warning 语义对齐
- 当前兼容行为保持不变
- 下一批可以直接讨论 `WithSNI(...)` / `TSSLConfig.ServerName` 的最终 public surface cleanup
