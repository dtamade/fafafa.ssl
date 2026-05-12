# sslCtxBoth Client Capability Clarification

## Goal
修复 `sslCtxBoth` 在“作为客户端使用”时的连接级能力漂移：当前多个后端的 connection 构造/early-data gate 仍把它当成“不是 client”，导致上下文默认 `ServerName` 丢失，且 early-data 直接在错误的 role 检查上被拒。

## Architecture
- 不改变 `sslCtxBoth` 枚举公开语义；`src/fafafa.ssl.base.pas` 仍声明它“同时支持客户端和服务端”。
- 本批只收口“client-capable runtime”：
  - connection 构造时，`sslCtxBoth` 应与 `sslCtxClient` 一样继承 context fallback `ServerName`
  - `ISSLEarlyDataConnection.SetEarlyData(...)` 的前置 role 检查应接受 `sslCtxBoth`
  - WolfSSL 的 client/server 预握手 OCSP 分支也要按 capability，而不是 strict equality
- 本批不重做更大的 “dual-role connection mode” 设计：
  - 不新增额外的 connection role state
  - 不先碰 `DoHandshakeInternal` 这类只有隐式握手路径才触发的行为
  - 如果 fresh RED 暴露更深的 dual-role 歧义，再开下一批

## Files
- Add: `tests/test_sslctxboth_client_capability_clarification.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`
- Modify: `src/fafafa.ssl.wolfssl.connection.pas`
- Modify: `src/fafafa.ssl.winssl.connection.pas`
- Modify: `src/fafafa.ssl.mbedtls.connection.pas`

## Steps
1. 写 focused RED：
   - 用 `sslCtxBoth` 创建 context
   - 证明 socket/stream 连接不会继承 context fallback `ServerName`
   - 对支持 early-data 的连接，证明 `SetEarlyData(...)` 先被错误的 “only available on client connections” gate 拒绝
2. 做最小 shared capability 修法：
   - 把 strict `sslCtxClient` / `sslCtxServer` 等值判断改成 capability 判断
   - 保持 `sslCtxServer` 不继承 client-only `ServerName`
3. 跑 focused GREEN 与相邻回归：
   - `tests/test_sslctxboth_client_capability_clarification.pas`
   - `tests/test_freepascal_context_server_name_inheritance.pas`
   - `tests/test_early_data_public_api_contract.pas`
   - 如涉及 OpenSSL/WolfSSL，再补对应 focused 回归
4. 更新 working-memory、review、commit。
