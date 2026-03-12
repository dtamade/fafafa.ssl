# TLS Connector Hostname Override Precedence Plan

**Goal**
- 收口 `TSSLConnector.ConnectSocket/ConnectStream(..., AServerName)` 的 hostname precedence：调用方传入的连接级 hostname 无论是非空覆盖还是空字符串清空，都要优先于 context fallback。

**Architecture**
- 当前 `TSSLConnector.ApplyClientOptions(...)` 只在 `AServerName <> ''` 时才调用 `ISSLClientConnection.SetServerName(...)`。
- 这会让 connection 在 `CreateConnection(...)` 阶段继承 context 默认 `ServerName` 后，无法被 `ConnectStream(..., '')` 显式清空。
- 这波沿用 builder precedence 的同一原则：调用方显式提供的 connection-level value（包括 empty string）优先于 inherited context fallback。

**Files**
- Add: `docs/plans/2026-03-09-tls-connector-hostname-override-precedence.md`
- Add: `tests/test_tls_connector_hostname_override_precedence.pas`
- Modify: `src/fafafa.ssl.tls.pas`
- Update: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 connector precedence focused RED。
2. 跑 RED，确认 empty hostname 无法清掉 inherited fallback。
3. 让 `ApplyClientOptions(...)` 对 empty hostname 也走 per-connection setter。
4. 跑 focused suites + compile-all。
5. 回写 working memory，并继续审查后续队列。

**Expected Outputs**
- `ConnectStream(..., 'override.example')` 覆盖 context fallback。
- `ConnectStream(..., '')` 显式清空 context fallback。
- connector / builder 在 hostname precedence 上保持一致：connection-level input 优先于 context fallback。
