# Connection Context ServerName Inheritance Parity Plan

**Goal**
- 收口不同 backend 在 `CreateConnection(...)` 时对 context 默认 `ServerName` 的继承语义。

**Architecture**
- 当前 OpenSSL / WinSSL / WolfSSL / MbedTLS 都会在 connection 构造时从 context 读取默认 `ServerName`，但 FreePascal 连接构造函数没有做这一步。
- 由于 `ISSLContext.GetServerName` 与 `ISSLClientConnection.GetServerName` 仍是现有公开 contract，这会让同一 context 配置在 FreePascal 后端 silently degrade。
- 这波先增加 focused cross-backend contract，再在 FreePascal connection 构造函数里做最小对齐，不重做更大层级的 SNI DSL 设计。

**Files**
- Add: `docs/plans/2026-03-09-connection-context-server-name-inheritance-parity.md`
- Add: `tests/test_connection_context_server_name_inheritance.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Update: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 context->connection `ServerName` focused RED。
2. 跑 RED，确认 FreePascal backend 没有继承 context 默认 `ServerName`。
3. 在 FreePascal connection 构造函数做最小修复。
4. 跑 focused suites + compile-all。
5. 回写 working memory 与下一队列。

**Expected Outputs**
- `CreateConnection(...)` 后的 `ISSLClientConnection.GetServerName` 在 FreePascal / OpenSSL 至少保持一致。
- context-level 默认 `ServerName` 不再只在部分 backend 生效。
