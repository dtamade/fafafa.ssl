# OpenSSL ServerName Clear Override Parity Plan

**Goal**
- 收口 OpenSSL backend 上 per-connection `SetServerName('')` 的清空语义，使字段状态与底层 native SNI 状态一致。

**Architecture**
- 当前 client connection 从 context 继承默认 `ServerName` 后，调用 `ISSLClientConnection.SetServerName('')` 会把 `FServerName` 清空，但 `GetConnectionInfo.ServerName` 仍返回旧值，说明 native `SSL` 句柄上的 SNI 没被清掉。
- 这会破坏我们刚刚合同化的 precedence 模型：`connection override > context default > empty`。如果显式空 override 不能落到底层，实现上仍然会偷偷走旧默认值。
- 这波先增加 focused OpenSSL contract，再在 `TOpenSSLConnection.SetServerName` 做最小修复，不扩散到其他 backend 的 native 清空 API 设计。

**Files**
- Add: `docs/plans/2026-03-09-openssl-server-name-clear-override-parity.md`
- Add: `tests/test_openssl_connection_server_name_clear_override.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`
- Update: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 OpenSSL focused RED，锁定 empty override 与 native state 不一致。
2. 跑 RED，确认 `GetConnectionInfo.ServerName` 仍保留旧值。
3. 在 `TOpenSSLConnection.SetServerName` 做最小清空修复。
4. 跑 focused suites + compile-all。
5. 回写 working memory 与下一队列。

**Expected Outputs**
- `ISSLClientConnection.GetServerName` 与 `GetConnectionInfo.ServerName` 在 OpenSSL client connection 上保持一致。
- 显式 `SetServerName('')` 可真正清掉 inherited/default SNI，而不是只改字段不改 native handle。
