# FreePascal Client Context SNI Fallback Cut

## Goal

把 `sslCtxClient` 在 FreePascal backend 上的 inherited context-level `ServerName` fallback 做成第一条真实 client-side behavior migration：新建连接后不再从 context 自动继承 deprecated SNI 状态，调用方必须显式走 `ISSLClientConnection.SetServerName(...)`。

## Architecture

- 只收 FreePascal runtime：
  - `src/fafafa.ssl.freepascal.connection.pas`
- 不碰：
  - shared shim 的其他 backend 路径
  - connector / connection-builder 的 precedence mock contracts
  - factory / builder 仍保留当前 compatibility warning / write surface
- 先让 dedicated FreePascal regression 从“继承旧 fallback”翻成“显式无继承”：
  - `tests/test_freepascal_context_server_name_inheritance.pas`
- 再用 focused source contract 守住 FreePascal 构造器不再读 shared compat shim：
  - `tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`

## Files

- Modify: `tests/test_freepascal_context_server_name_inheritance.pas`
- Add: `tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify: `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- Update: `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. Turn the dedicated FreePascal regression into RED:
   - builder client path should no longer inherit `WithSNI(...)`
   - direct context path should no longer inherit `SetServerName(...)`
2. Add a focused source contract:
   - fail if `src/fafafa.ssl.freepascal.connection.pas` still reads `GetContextLevelServerNameCompatibilityValue(...)`
3. Observe RED before production edits.
4. Remove the compatibility read from the two FreePascal client constructors.
5. Re-run focused verification:
   - `bash tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`
   - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
   - `tests/test_freepascal_context_server_name_inheritance.pas`
6. Close out docs / working memory and record the next recommended batch.

## Expected Outputs

- FreePascal client socket/stream connections stop inheriting deprecated context-level `ServerName`
- `tests/test_freepascal_context_server_name_inheritance.pas` no longer belongs to the intentional compatibility label set
- next remaining intentional compatibility surface shrinks to precedence/connector mock contracts plus the server builder compatibility test
