# Connection Server Mode SNI Isolation Plan

**Goal**
- 收口 `CreateConnection(...)` 时 `ServerName` 默认值只应服务于 client connection，不应从 server context 自动下沉到 server-side connection。

**Architecture**
- 目前各 backend 都会在 connection 构造或初始化阶段读取 context 默认 `ServerName`；这对 client backward compatibility 有价值，但对 server context 属于 client-only SNI 语义泄漏。
- `ISSLContext.SetServerName/GetServerName` 已被标记为 deprecated，推荐走 `ISSLClientConnection.SetServerName/GetServerName`；同时 `ISSLClientConnection` 文档明确其职责是 per-connection、client-specific 设置。
- 这波先增加 focused contract：client context 继续继承 context 默认 `ServerName`，server context 则不把该默认值注入新连接。然后在各 backend 上做最小守卫，不重做更大的 API 分层。

**Files**
- Add: `docs/plans/2026-03-09-connection-server-mode-sni-isolation.md`
- Add: `tests/test_connection_server_mode_sni_isolation.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`
- Modify: `src/fafafa.ssl.winssl.connection.pas`
- Modify: `src/fafafa.ssl.wolfssl.connection.pas`
- Modify: `src/fafafa.ssl.mbedtls.connection.pas`
- Update: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 client inherit / server isolate focused RED。
2. 跑 RED，确认 server context 目前错误地下沉了默认 `ServerName`。
3. 在各 backend connection 初始化路径加最小 client-only 守卫。
4. 跑 focused suites + compile-all。
5. 回写 working memory 与下一队列。

**Expected Outputs**
- client context 创建的连接仍能继承 context 默认 `ServerName`。
- server context 创建的连接不再自动携带 client-only `ServerName`。
