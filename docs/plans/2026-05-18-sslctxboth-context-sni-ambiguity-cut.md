# sslCtxBoth Context SNI Ambiguity Cut

## Goal

让 `sslCtxBoth` 与它现有的“角色不可猜测”原则一致：双角色 context 仍可创建 client-capable connection，但不再把 deprecated context-level `ServerName` 自动继承成 client fallback。

## Architecture

- 范围只限 shared compatibility shim：
  - `sslCtxClient` 的 direct-context / factory / builder client fallback 暂不改
  - `sslCtxBoth` 的 role-less handshake / early-data role gate 语义保持不变
- 目标：
  - `sslCtxBoth` 仍 exposes `ISSLClientConnection`
  - 但 inherited context-level `ServerName` fallback 变为空
  - 调用方如果真要走 client role，必须在 connection 上显式 `SetServerName(...)`

## Files

- `src/fafafa.ssl.context.compat.pas`
- `tests/test_sslctxboth_client_capability_clarification.pas`
- `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- `tests/test_sslctxboth_roleless_handshake_clarification.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. focused RED:
   - `test_sslctxboth_client_capability_clarification` 改成新预期：
     - dual-context connection 仍 exposes `ISSLClientConnection`
     - 但不再继承 context-level `ServerName`
   - intentional-compat labels contract 移除该文件
2. production change:
   - shared compatibility shim 对 `sslCtxBoth` 返回空字符串
3. focused verification:
   - `tests/test_sslctxboth_client_capability_clarification.pas`
   - `tests/test_sslctxboth_roleless_handshake_clarification.pas`
   - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
4. closeout:
   - 更新路线图与记录，明确这是第一条 client-side fallback migration cut

## Expected Outputs

- `sslCtxBoth` 不再一边要求显式选择握手角色，一边又静默继承 client-side SNI fallback
- 下一批 client-side 行为迁移可以继续围绕 `sslCtxClient` 直接 fallback 收缩
