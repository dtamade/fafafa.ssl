# `GetConnectionInfo` PeerCertificate Base Enrichment

## Goal

把 `GetConnectionInfo` 剩余 completeness debt 里最适合共享层统一补齐的一项继续落下：让 `TBaseSSLConnection.GetConnectionInfo` 在连接可暴露当前对端证书时补齐 `PeerCertificate`，避免非 OpenSSL/WinSSL 后端继续把这项 metadata 留空。

## Scope

本批只处理共享连接层、focused contract、文档说明与台账：

- `src/fafafa.ssl.connection.base.pas`
- `tests/test_connection_builder_hostname_precedence.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不在本批补 `CipherSuiteId` / `KeyExchange` / `Cipher` / `Hash` / `KeySize` / `MacSize`
- 不开新的 backend-specific cipher mapping 重构
- 不重跑整条 backend contract / minimal CI gate

## Why This Batch

静态盘点当前 `GetConnectionInfo` 实现后，`PeerCertificate` 明显和 `ServerName` / `SessionId` 一样，属于“共享层已经有稳定来源，但还没折进 record”的字段：

- OpenSSL / WinSSL / FreePascal / MbedTLS / WolfSSL 都已经能通过 `DoGetPeerCertificate` 暴露当前对端证书
- 各后端对应的 `ISSLCertificate.GetInfo` 也都已经存在
- 当前只有 WinSSL override 会显式把 `PeerCertificate` 写回 `TSSLConnectionInfo`
- 这说明缺口不是“有没有底层能力”，而是 shared layer 还没有统一补齐

## Planned Changes

1. 在 focused mock contract 里先加 shared-truth proof：
   - `ConnectionInfo.PeerCertificate.Subject` 应镜像 `ISSLCertificate.GetInfo.Subject`
   - `ConnectionInfo.PeerCertificate.Issuer` 应镜像 `ISSLCertificate.GetInfo.Issuer`
2. 修改 `TBaseSSLConnection.GetConnectionInfo`，在 `GetPeerCertificate <> nil` 时统一补齐 `PeerCertificate`
3. 同步 `API_REFERENCE.md`：
   - 把 `PeerCertificate` 从“完全 backend-specific best-effort”收窄成“连接可暴露当前对端证书时由共享层补齐”

## Verification

```bash
mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence
mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract
bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh
git diff --check
```

## Execution Result

- focused mock proof:
  - `tests/test_connection_builder_hostname_precedence.pas`
  - result:
    - `15 passed, 0 failed`
    - `ConnectionInfo.PeerCertificate.Subject` 已镜像 `ISSLCertificate.GetInfo.Subject`
    - `ConnectionInfo.PeerCertificate.Issuer` 已镜像 `ISSLCertificate.GetInfo.Issuer`

- focused OpenSSL guard proof:
  - `tests/test_openssl_connection_info_cipher_contract.pas`
  - result:
    - `10 passed, 0 failed`
    - fresh-connection path 在引入 shared `GetPeerCertificate` 读取后仍保持安全

- residual contract follow-up:
  - `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result:
    - current intentional direct-core allowlist stayed unchanged
    - no new residual file had to be carved out for this batch

## Expected Outcome

- `PeerCertificate` 不再只在 WinSSL override 路径有机会被写进 `TSSLConnectionInfo`
- shared `GetConnectionInfo` completeness 再推进一格，而不必先进入后端专属 cipher 细节泥潭
- 下一批真正剩下的 completeness debt 会更聚焦到：
  - `CipherSuiteId`
  - `KeyExchange`
  - `Cipher`
  - `Hash`
  - `KeySize`
  - `MacSize`
