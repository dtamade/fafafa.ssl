# `GetConnectionInfo` Base Enrichment From Residual Audit

## Goal

把 `GetConnectionInfo` residual 审查里已经暴露出来、且可以由共享连接层统一补齐的 metadata 真实修掉：让 `TBaseSSLConnection.GetConnectionInfo` 至少补上 `ServerName` 与 `SessionId`，避免各后端在这些通用字段上继续出现“实现明明有信息，但 connection info record 仍为空”的缺口。

## Scope

本批只处理一条共享基类实现、一个 focused mock test、文档说明与台账：

- `src/fafafa.ssl.connection.base.pas`
- `tests/test_connection_builder_hostname_precedence.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不开新的 backend-specific runtime patch
- 不承诺在本批一次补齐 `PeerCertificate` / `KeyExchange` / `Hash` / `MacSize`
- 不重跑整条 backend contract / minimal CI gate

## Why This Batch

在 `GetConnectionInfo` residual freeze 之后，静态实现审查暴露出一个更实在的 completeness gap：

- `TSSLConnectionInfo` 文档仍把自己描述成“完整信息”
- 但共享基类 `TBaseSSLConnection.GetConnectionInfo` 当前只填了最小字段
- 对于所有实现了 `ISSLClientConnection` / `ISSLSession` 的后端来说，`ServerName` 与 `SessionId` 其实已经可以从共享层统一补齐

这意味着当前缺口不是某个单独 backend 的 override 漂移，而是共享连接层还没有把现成 metadata 折叠进 `TSSLConnectionInfo`。

## Planned Changes

1. 先在 focused mock test 里加 RED：
   - `ConnectionInfo.ServerName` 应镜像 `ISSLClientConnection.GetServerName`
   - `ConnectionInfo.SessionId` 应镜像 `ISSLSession.GetID`
2. 仅在 RED 成立后，修改 `TBaseSSLConnection.GetConnectionInfo`，统一补齐这两项字段。
3. 同步 `API_REFERENCE.md`，把 `TSSLConnectionInfo` 改成更符合当前跨后端 truth 的说明：
   - 通用字段由共享连接层保证
   - 其余 backend-specific 字段 best-effort 填充

## Verification

```bash
mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence
mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract
bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh
git diff --check
```

## Execution Result

- focused mock proof：
  - `tests/test_connection_builder_hostname_precedence.pas`
  - result:
    - `13 passed, 0 failed`
    - `ConnectionInfo.ServerName` 已镜像 `ISSLClientConnection.GetServerName`
    - `ConnectionInfo.SessionId` 已镜像 `ISSLSession.GetID`

- focused OpenSSL guard proof：
  - `tests/test_openssl_connection_info_cipher_contract.pas`
  - result:
    - `10 passed, 0 failed`
    - fresh-connection path 不再复现先前的 `EAccessViolation`

- implementation decision captured by this batch：
  - shared base 不使用 `Supports(Self, ISSLClientConnection, ...)`
  - 改用 `DoGetConnectionInfoServerName` protected virtual hook
  - root cause:
    - concrete-object construction + temporary interface ref 在 `TInterfacedObject` 路径上有错误自释放风险

- residual contract follow-up：
  - `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result:
    - allowlist expected hit count 从 `7` 更新到 `9`
    - `tests/test_connection_builder_hostname_precedence.pas` 被纳入 intentional direct-core proof file set

## Expected Outcome

- shared `GetConnectionInfo` truth becomes less hollow without touching backend-specific code
- `ServerName` / `SessionId` stop drifting empty when the connection already holds that metadata
- next batch can focus on the truly remaining completeness debt:
  - `PeerCertificate`
  - backend-specific crypto detail fields
  - stronger owner/deprecation route
