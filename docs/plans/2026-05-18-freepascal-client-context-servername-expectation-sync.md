# FreePascal Client Context ServerName Expectation Sync

## Goal

修正三份已经被 FreePascal client runtime fallback cut 甩开的 focused contracts：这些测试仍把 `BuildClient.WithSNI(...)` / factory client `ServerName` compatibility surface 描述成“新连接会继承 deprecated context-level ServerName”，但当前 FreePascal runtime 早已不再这样工作。

## Architecture

- 不改生产代码：
  - `src/fafafa.ssl.freepascal.connection.pas` 已经明确“不再从 context 继承 client ServerName”
- 只修测试/路线真相：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_factory_server_name_scope_clarification.pas`
  - `tests/test_factory_config_server_name_isolation.pas`
- 同步路线记录：
  - `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps

1. Turn the stale FreePascal expectations into RED evidence:
   - `BuildClient.WithSNI(...)` still preserves deprecated context state, but FreePascal connections no longer inherit it
   - factory client `ServerName` config still preserves deprecated context state, but FreePascal connections no longer inherit it
2. Update the three focused tests so they describe the actual current boundary.
3. Re-run focused verification:
   - `tests/test_context_builder_server_servername_runtime_consistency.pas`
   - `tests/test_factory_server_name_scope_clarification.pas`
   - `tests/test_factory_config_server_name_isolation.pas`
   - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
4. Close out docs / working memory and move the next recommendation to the remaining shared client-fallback backends instead of the old server-side control case.

## Expected Outputs

- FreePascal-focused contracts stop teaching inherited context-level client fallback that no longer exists
- route truth explicitly records that the next real implementation question is whether OpenSSL / WolfSSL / MbedTLS / WinSSL should follow the same no-inheritance rule
