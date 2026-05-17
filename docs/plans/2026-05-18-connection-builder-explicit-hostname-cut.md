# Connection Builder Explicit Hostname Cut

## Goal

把 `TSSLConnectionBuilder` 的 client hostname 语义继续从 inherited context fallback 收紧到 explicit per-connection semantics：未调用 `WithHostname(...)` 时，也不再保留 context-level `ServerName` fallback。

## Architecture

- 目标生产面：
  - `src/fafafa.ssl.connection.builder.pas`
- 目标契约面：
  - `tests/test_connection_builder_hostname_precedence.pas`
- 不碰：
  - `TSSLConnector` 的 override precedence
  - 其他 backend constructor / shared compat shim
  - server-side builder behavior

## Files

- Add: `docs/plans/2026-05-18-connection-builder-explicit-hostname-cut.md`
- Modify: `tests/test_connection_builder_hostname_precedence.pas`
- Modify: `src/fafafa.ssl.connection.builder.pas`
- Modify: `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- Update: `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. Turn `tests/test_connection_builder_hostname_precedence.pas` into RED:
   - case 1 should no longer preserve context fallback
   - case 2 should still allow explicit override
   - case 3 should still allow explicit empty clear
2. Remove the test from the intentional compatibility label set.
3. Observe RED before production edits.
4. Update `TSSLConnectionBuilderImpl.TryBuildClient`:
   - if the built connection supports `ISSLClientConnection`
   - and no explicit hostname was provided
   - clear `ServerName` to `''` so inherited context fallback does not survive the builder path
5. Re-run focused verification:
   - `tests/test_connection_builder_hostname_precedence.pas`
   - `tests/test_tls_connector_hostname_override_precedence.pas`
   - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
6. Close out docs / working memory and record the next recommended batch.

## Expected Outputs

- `TSSLConnectionBuilder` client path no longer preserves inherited context-level `ServerName`
- `tests/test_connection_builder_hostname_precedence.pas` leaves the intentional compatibility set
- next remaining client-side intentional compatibility surface shrinks to connector override precedence plus the server builder compatibility test
