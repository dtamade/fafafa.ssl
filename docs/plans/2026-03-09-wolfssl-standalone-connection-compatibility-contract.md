# WolfSSL Standalone Connection Compatibility Contract Plan

**Goal**
- 让 `src/fafafa.ssl.wolfssl.connection.pas` 不再维护第二套完整实现，而是作为薄兼容层委托真实 runtime path。
- 让直接 `uses fafafa.ssl.wolfssl.connection` 的调用方同时继承 `ServerName` 与 native handle 的同一份契约。

**Architecture**
- 当前 public runtime path 由 `TWolfSSLContext.CreateConnection(...)` 驱动，真实连接对象定义在 `src/fafafa.ssl.wolfssl.context.pas`。
- 仓内同时存在 `src/fafafa.ssl.wolfssl.connection.pas` 的历史公开类名 `TWolfSSLConnection`；它会被 compile-all 编译，也可能被外部调用方直接引用。
- 如果继续维护两套完整实现，SNI / `GetConnectionInfo.ServerName` / `ISSLNativeHandleAccess` 会持续发生语义漂移。
- 这波采用更稳的 root fix：把 standalone 单元改成 compatibility shim，直接委托 `AContext.CreateConnection(...)` 返回的真实 runtime 连接对象。

**Files**
- Add: `docs/plans/2026-03-09-wolfssl-standalone-connection-compatibility-contract.md`
- Add: `tests/test_wolfssl_standalone_connection_server_name_compatibility.pas`
- Add: `tests/test_wolfssl_standalone_native_handle_compatibility.pas`
- Modify: `src/fafafa.ssl.wolfssl.connection.pas`
- Update: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 standalone `ServerName` compatibility RED。
2. 增加 standalone native handle compatibility RED。
3. 将 standalone 单元收口为委托 runtime path 的薄兼容层。
4. 跑 focused suites + compile-all + diff check。
5. 回写 working memory，并把下一波改为“兼容层策略 + 重复路径审计”。

**Expected Outputs**
- 直接 `uses fafafa.ssl.wolfssl.connection` 的调用方能得到与 runtime path 一致的 `ServerName` create/override/clear 行为。
- standalone 单元不再在 `ISSLClientConnection`、`GetConnectionInfo.ServerName`、`ISSLNativeHandleAccess` 上落后于 runtime path。
- WolfSSL 连接语义重新收敛为单一真相源；standalone 单元只承担兼容入口，而不是第二套运行时实现。
