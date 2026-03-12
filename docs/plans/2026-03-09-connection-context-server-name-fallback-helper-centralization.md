# Connection Context ServerName Fallback Helper Centralization Plan

**Goal**
- 把运行连接层读取 deprecated context-default `ServerName` 的逻辑收口到一条共享 helper。
- 避免 OpenSSL / FreePascal / WinSSL / MbedTLS 在各自构造或初始化路径里继续复制 `AContext.GetServerName` fallback。

**Architecture**
- 这条 fallback 仍然是 backward-compatible 语义，不是新的推荐 API；真正的 per-connection SNI 仍由 `ISSLClientConnection.SetServerName(...)` 主导。
- 本波不改变各 backend “如何应用” `ServerName`：OpenSSL 仍通过 `SetServerName(...)` 同步 native handle，FreePascal / WinSSL / MbedTLS 仍保留各自 backend-specific 行为。
- 共享的只是“何时/从哪里读取 deprecated context default”这一层：统一放进 `TBaseSSLConnection.GetLegacyContextDefaultServerName`。
- 这样能把后续 drift 风险从 4 份 runtime 入口压缩成 1 份共享 helper。

**Files**
- Add: `docs/plans/2026-03-09-connection-context-server-name-fallback-helper-centralization.md`
- Add: `tests/scripts/test_connection_context_server_name_fallback_helper_contract.sh`
- Modify: `src/fafafa.ssl.connection.base.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify: `src/fafafa.ssl.winssl.connection.pas`
- Modify: `src/fafafa.ssl.mbedtls.connection.pas`
- Update: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加结构合同，要求 runtime 连接路径使用共享 helper。
2. 跑 RED，确认各 backend 仍在复制 direct deprecated fallback。
3. 在 `TBaseSSLConnection` 增加共享 helper。
4. 让 OpenSSL / FreePascal / WinSSL / MbedTLS 接入 helper。
5. 跑 focused contracts + runtime regressions + compile-all + diff check。

**Expected Outputs**
- runtime 连接层不再散落 direct `AContext.GetServerName` fallback。
- backward-compatible context default 语义保持不变。
- 后续若调整 deprecated fallback，只需改共享 helper，而不是逐 backend 手工追 4 处路径。
