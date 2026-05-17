# Residual Context SNI Classification And mTLS Skeleton Cleanup

## Goal

收掉当前活跃 `context-level SetServerName(...)` 残余面里最小但最模糊的一批文件：给仍属 intentional compatibility / API-surface coverage 的文件补显式标签，并把 `test_winssl_mtls_skeleton.pas` 里还属于普通握手流的 context-level SNI 迁到 per-connection。

## Architecture

- label-only files:
  - `tests/test_tls_connector_early_data_contract.pas`
  - `tests/mbedtls/test_mbedtls_context_contract.pas`
  - `tests/wolfssl/test_wolfssl_context_contract.pas`
  - `tests/winssl/test_winssl_library_basic.pas`
- mixed file:
  - `tests/winssl/test_winssl_mtls_skeleton.pas`
    - 配置段里的 `SetServerName('test.example.com')` 归类为 API-surface coverage
    - 真实 `TestMTLSHandshake` 路径里的 `SetServerName(ServerHost)` 迁到 connection-level SNI
- focused verification：
  - source contract 守住标签与握手流迁移
  - Linux-safe files 直接编译
  - WinSSL files 尝试 Win64 交叉编译

## Files

- `tests/scripts/test_residual_context_sni_classification_contract.sh`
- `tests/test_tls_connector_early_data_contract.pas`
- `tests/mbedtls/test_mbedtls_context_contract.pas`
- `tests/wolfssl/test_wolfssl_context_contract.pas`
- `tests/winssl/test_winssl_library_basic.pas`
- `tests/winssl/test_winssl_mtls_skeleton.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused RED contract：
   - 上述 label-only 文件必须显式包含对应 `INTENTIONAL_*` 标签
   - `test_winssl_mtls_skeleton.pas` 不得继续在真实握手流里使用 `Ctx.SetServerName(ServerHost)`
2. 生产修复：
   - 补显式标签
   - 把 `TestMTLSHandshake` 迁到 per-connection SNI
3. focused verification：
   - `bash tests/scripts/test_residual_context_sni_classification_contract.sh`
   - `tests/test_tls_connector_early_data_contract.pas`
   - `tests/mbedtls/test_mbedtls_context_contract.pas`
   - `tests/wolfssl/test_wolfssl_context_contract.pas`
   - Win64 交叉编译 `tests/winssl/test_winssl_library_basic.pas`
   - Win64 交叉编译 `tests/winssl/test_winssl_mtls_skeleton.pas`
4. 收口：
   - 更新路线图，明确剩余活跃 context-level `SetServerName(...)` 更接近“全部 intentional”

## Expected Outputs

- 剩余残余文件不再处于“看起来像普通用法但其实是 intentional coverage”的模糊状态
- `test_winssl_mtls_skeleton.pas` 的真实握手流不再依赖 context-level SNI
- 下一批更容易直接选择第一条真正的 behavior migration RED

## Closeout

- residual 分类与 WinSSL mTLS skeleton 握手迁移已完成
- `tests/scripts/test_residual_context_sni_classification_contract.sh` 已转绿
- Linux-safe focused compile 与 Win64 focused cross-compile 已通过
