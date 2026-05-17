# BuildServer WithSNI Ignore Behavior Migration

## Goal

把 `TSSLContextBuilder.BuildServer` 的 `WithSNI(...)` 从“warning 里说会被忽略、但 runtime 仍把它写进 context”收成一致真相：server builder 继续保留 compatibility warning，但 built server context 不再保留这份 client-only `ServerName` 状态。

## Architecture

- 这是第一条真正的 behavior migration cut，但范围只限 server-side builder：
  - 不改 client-side context fallback
  - 不改 direct context `SetServerName(...)` 的 intentional compatibility coverage
  - 不改 backend constructor fallback
- 目标是一致化三层语义：
  - runtime: `BuildServer` 不再 `Result.SetServerName(FServerName)`
  - validation: 明确 `WithSNI(...)` 在 server builder 上会被忽略
  - warning/docs: 不再暗示它仍会被“应用”

## Files

- `tests/test_context_builder_server_servername_runtime_consistency.pas`
- `tests/test_context_builder_server_name_compatibility_warning.pas`
- `tests/config/test_config_validation.pas`
- `src/fafafa.ssl.context.builder.pas`
- `docs/reference/API_REFERENCE.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. focused RED:
   - 把 `test_context_builder_server_servername_runtime_consistency` 改成新预期：
     - `BuildServer.WithSNI(...)` 不再让 built context 保留 `ServerName`
     - server connection 仍然不继承它
   - 同步 warning / validation 测试到 “ignored on BuildServer” 术语
2. production change:
   - `BuildServer` 保留 warning，但不再调用 `Result.SetServerName(FServerName)`
   - validation / warning wording 改成 ignore 语义
3. focused verification:
   - `tests/test_context_builder_server_servername_runtime_consistency.pas`
   - `tests/test_context_builder_server_name_compatibility_warning.pas`
   - `tests/config/test_config_validation.pas`
4. closeout:
   - 更新路线图与工作记录，明确这是第一条 server-side behavior migration cut

## Expected Outputs

- `BuildServer.WithSNI(...)` 不再制造“context 里有值、server connection 又忽略它”的 runtime 分裂
- builder warning / validation / runtime 术语重新一致
- 下一批可以继续选择真正的 client-side fallback behavior migration，而不是继续处理 server-only dead compatibility
