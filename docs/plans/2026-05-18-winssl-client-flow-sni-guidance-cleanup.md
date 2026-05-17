# WinSSL Client Flow SNI Guidance Cleanup

## Goal

从一组明确属于普通 WinSSL 客户端连接流的测试里移除 deprecated context-level `SetServerName(...)` 用法，改成 per-connection SNI 设置，为后续真正的 compatibility behavior migration 铺路。

## Architecture

- 只处理正常客户端连接流，不碰 intentional compatibility / API-surface coverage：
  - `tests/winssl/test_winssl_error_mapping_online.pas`
  - `tests/winssl/test_winssl_https_client.pas`
  - `tests/winssl/test_winssl_revocation_online.pas`
  - `tests/winssl/test_winssl_mtls_e2e_local.pas`
- 保持原断言目标不变：
  - 证书错误映射
  - HTTPS 基本连通
  - 吊销校验
  - 本地 mTLS 握手
- focused verification：
  - shell contract 守住“这些普通连接流不得再教 context-level SNI”
  - 能在本地编译的文件就编译；需要 Win64/Windows 的验证则如实记录

## Files

- `tests/scripts/test_winssl_client_flow_tests_no_context_level_sni_guidance_contract.sh`
- `tests/winssl/test_winssl_error_mapping_online.pas`
- `tests/winssl/test_winssl_https_client.pas`
- `tests/winssl/test_winssl_revocation_online.pas`
- `tests/winssl/test_winssl_mtls_e2e_local.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused RED contract：
   - 上述四个文件不得继续用 `Context/Ctx/LCtx/LContext.SetServerName(...)`
2. 生产修复：
   - 在 `CreateConnection(...)` 之后
   - cast 到 `ISSLClientConnection`
   - 在 `Connect` / `DoHandshake` 前设置 `ServerName`
3. focused verification：
   - `bash tests/scripts/test_winssl_client_flow_tests_no_context_level_sni_guidance_contract.sh`
   - 尝试本地可行编译；若 Win64/Windows 工具链受限，则在 `progress.md` 里明确写实
4. 收口：
   - 记录这些文件属于 normal client-flow guidance，不属于 intentional compatibility / API-surface coverage

## Expected Outputs

- 这四个 WinSSL 客户端流测试不再依赖 deprecated context-level SNI
- 剩余 context-level `SetServerName(...)` 活跃用法的分类会更清晰
- 下一批可以继续挑真实客户端流，而不是重新碰已确认的 API-surface / compatibility coverage
