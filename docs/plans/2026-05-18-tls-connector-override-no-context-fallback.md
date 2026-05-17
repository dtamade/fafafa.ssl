# TLS Connector Override Without Context Fallback

## Goal

把 `tests/test_tls_connector_hostname_override_precedence.pas` 从 intentional compatibility 输入中移除：该契约真正要锁的是 connector 的连接级 override 语义，而不是 inherited context fallback 本身。

## Architecture

- 不改生产代码：
  - `src/fafafa.ssl.tls.pas` 已经是纯 per-connection `SetServerName(...)` 路径
- 只改测试/合同：
  - `tests/test_tls_connector_hostname_override_precedence.pas`
  - `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - `tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`

## Files

- Add: `docs/plans/2026-05-18-tls-connector-override-no-context-fallback.md`
- Add: `tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`
- Modify: `tests/test_tls_connector_hostname_override_precedence.pas`
- Modify: `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- Update: `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. Add a focused source contract:
   - fail if `tests/test_tls_connector_hostname_override_precedence.pas` still teaches `Ctx.SetServerName(...)`
2. Remove the deprecated context-level SNI setup from that test.
3. Remove the test from the intentional compatibility label set.
4. Re-run focused verification:
   - `bash tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`
   - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
   - `tests/test_tls_connector_hostname_override_precedence.pas`
5. Close out docs / working memory and record the next recommended batch.

## Expected Outputs

- connector override precedence contract no longer depends on inherited context fallback
- remaining intentional compatibility label set shrinks again
- next client-side intentional input can move to `tests/test_tls_connector_early_data_contract.pas`
