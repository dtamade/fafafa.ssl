# TLS Connector Early-Data Without Context Fallback

## Goal

把 `tests/test_tls_connector_early_data_contract.pas` 从“故意从 inherited context fallback 起步”的旧叙述中移出；该契约真正要锁的是 connector 在显式 hostname 下的 session/servername/early-data/connect 顺序与失败语义。

## Architecture

- 不改生产代码：
  - `src/fafafa.ssl.tls.pas` 的 `TSSLConnector.ApplyClientOptions(...)` 已明确采用连接级 `SetServerName(AServerName)`
  - `TryQueueEarlyData(...)` 与 context-level `ServerName` 无关
- 只改测试/合同：
  - `tests/test_tls_connector_early_data_contract.pas`
  - `tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`

## Files

- Add: `docs/plans/2026-05-18-tls-connector-early-data-no-context-fallback.md`
- Add: `tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
- Modify: `tests/test_tls_connector_early_data_contract.pas`
- Update: `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. Remove the deprecated context-level SNI setup from the early-data contract.
2. Reword the assertion text so the test describes explicit per-connection hostname application instead of overriding inherited fallback.
3. Add a focused source contract that fails if this test regresses back to `Ctx.SetServerName(...)`.
4. Re-run focused verification:
   - `bash tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
   - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
   - `tests/test_tls_connector_early_data_contract.pas`
5. Close out docs / working memory and move the next recommendation to the last intentional server-side control case.

## Expected Outputs

- connector early-data contract no longer depends on inherited context fallback
- remaining intentional compatibility label set stays reduced to the server-side control case
- next bounded review can move to `tests/test_context_builder_server_servername_runtime_consistency.pas`
