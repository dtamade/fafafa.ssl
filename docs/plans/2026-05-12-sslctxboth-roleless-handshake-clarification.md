# sslCtxBoth Roleless Handshake Clarification

## Goal
修复 `sslCtxBoth` 在 role-less 握手入口上的公共合同漂移：`ISSLConnection.DoHandshake` 没有 client/server 参数，但当前不同后端会静默猜测角色；OpenSSL 的 stream `Read/Write` 还会在未连接时隐式握手，同样对 `sslCtxBoth` 存在歧义。

## Architecture
- `sslCtxBoth` 继续保留为“双向能力 context”：
  - 显式 `Connect` 仍代表 client
  - 显式 `Accept` 仍代表 server
- 但 role-less 入口没有足够信息：
  - `ISSLConnection.DoHandshake`
  - OpenSSL stream transport 上未连接时的隐式 `Read/Write`
- 这批不发明新的 connection role state，也不默认猜一边。
- 最小安全修法是 fail-fast：
  - `DoHandshake` 遇到 `sslCtxBoth` 时返回 `sslHsFailed`
  - connection health/error surface 记录清晰 `sslErrConfiguration`
  - OpenSSL implicit stream handshake 在 `sslCtxBoth` 下直接拒绝，要求先显式 `Connect` 或 `Accept`

## Files
- Add: `tests/test_sslctxboth_roleless_handshake_clarification.pas`
- Modify: `src/fafafa.ssl.connection.base.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`

## Steps
1. 写 focused RED：
   - `sslCtxBoth` 连接调用 `DoHandshake`
   - 断言不该再静默猜角色，而应给出明确 configuration error
   - OpenSSL stream `Read/Write` 未连接时对 `sslCtxBoth` 也应 fail-fast
2. 做最小 shared fix：
   - 在基类 `DoHandshake` 收 role-less dual-role 歧义
   - 在 OpenSSL implicit stream handshake 入口补同样的 precondition
3. 跑 focused GREEN 与相邻回归：
   - `tests/test_sslctxboth_roleless_handshake_clarification.pas`
   - `tests/test_openssl_connection_stream_handshake_contract.pas`
   - `tests/test_sslctxboth_client_capability_clarification.pas`
   - `python3 scripts/compile_all_modules.py`
4. 更新 working-memory、review、commit。
