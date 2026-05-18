# Active Direct Context ServerName Surface Classification

## Goal

把 active tests 里剩余的 direct `ISSLContext.SetServerName(...)` 使用面一次性分类清楚：

- 所有活跃 `Ctx.SetServerName(...)` 命中都必须是显式的 `INTENTIONAL_COMPAT` 或 `INTENTIONAL_API_SURFACE`
- 不再允许未归类的普通测试偷偷保留 direct context ServerName 指导语义
- 用一个 repo-level focused contract 守住这份活跃 direct-context compatibility/API-surface 清单

## Architecture

- 这是纯静态、纯护栏批次，不改 runtime 行为
- 对 remaining compatibility contracts 补文件级 `INTENTIONAL_COMPAT`
- 新增一份 active-tests 总分类合同：
  - allowlist 中的 direct-context 命中必须带正确分类标签
  - allowlist 外的 active test 一旦重新出现 direct context `SetServerName(...)`，直接红灯

## Files

- Add: `docs/plans/2026-05-18-active-direct-context-servername-surface-classification.md`
- Add: `tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
- Update:
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - `tests/test_sslctxboth_client_capability_clarification.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
- Update:
  - `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps

1. Enumerate remaining active direct-context `SetServerName(...)` hits.
2. Add explicit `INTENTIONAL_COMPAT` labels to the uncovered compatibility tests.
3. Add one focused contract that classifies the entire active direct-context surface.
4. Re-run:
   - the new contract
   - one cross-backend clarification regression
   - one builder-precedence clarification regression
   - one `sslCtxBoth` clarification regression

## Expected Outputs

- Active direct-context ServerName surface becomes fully searchable and explicitly classified
- No ordinary test can silently reintroduce unlabelled `Ctx.SetServerName(...)`
- Next batch can move straight to final API-shape decisions for:
  - `TSSLConfig.ServerName`
  - `WithSNI(...)`
  - direct `ISSLContext.SetServerName/GetServerName`
