# High-Level Context ServerName Ignore Cut

## Goal

把最后一层高层遗留语义收紧：`TSSLContextBuilder.WithSNI(...)` 与 `TSSLFactory.CreateContext(... ServerName ...)` 继续接受旧输入并发出明确 warning，但不再把 deprecated `context-level ServerName` 写回新建 context。

## Architecture

- 保留：
  - direct `ISSLContext.SetServerName/GetServerName` compatibility surface
  - server-side reject / ignore 规则
  - builder import/export/config snapshot 对 `server_name` 的兼容载荷
- 收紧：
  - `BuildClient` 不再把 `FServerName` 写进 built context
  - factory client default-config / one-shot path 不再把 `TSSLConfig.ServerName` 写进 built context
  - validation / warning / API note 要同步改成 “warning + ignore”

## Files

- Add: `docs/plans/2026-05-18-high-level-context-servername-ignore-cut.md`
- Update: `tests/test_context_builder_server_servername_runtime_consistency.pas`
- Update: `tests/test_factory_server_name_scope_clarification.pas`
- Update: `tests/test_factory_config_server_name_isolation.pas`
- Update: `tests/test_context_builder_server_name_compatibility_warning.pas`
- Update: `tests/test_factory_server_name_compatibility_warning.pas`
- Update: `tests/config/test_config_validation.pas`
- Update: `src/fafafa.ssl.context.builder.pas`
- Update: `src/fafafa.ssl.factory.pas`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. Flip the focused builder/factory runtime contracts to the new expectation:
   - high-level inputs are accepted
   - warnings are emitted
   - built contexts keep `GetServerName = ''`
2. Observe RED on:
   - `tests/test_context_builder_server_servername_runtime_consistency.pas`
   - `tests/test_factory_server_name_scope_clarification.pas`
   - `tests/test_factory_config_server_name_isolation.pas`
   - `tests/test_context_builder_server_name_compatibility_warning.pas`
   - `tests/test_factory_server_name_compatibility_warning.pas`
3. Update production/runtime truth:
   - `src/fafafa.ssl.context.builder.pas`
   - `src/fafafa.ssl.factory.pas`
   - validation wording where needed
4. Re-run focused verification plus docs/plan sync.

## Expected Outputs

- `BuildClient.WithSNI(...)` becomes warning + ignore
- `TSSLFactory.CreateContext(...)` client-side `TSSLConfig.ServerName` becomes warning + ignore
- direct `ISSLContext.SetServerName/GetServerName` remains the only observable context-level compatibility surface
- roadmap progress moves from “last direct server-context control case pending” to “high-level write surface cut complete”
