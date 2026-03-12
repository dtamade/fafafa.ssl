# WolfSSL / MbedTLS ServerName Observability Parity Plan

**Goal**
- 让 WolfSSL / MbedTLS 的真实运行连接类具备与 OpenSSL 一致的 `ServerName` 可观测性，并修复 clear-path parity。

**Architecture**
- focused RED 暴露出更深的根因：
  - WolfSSL 真实运行路径走 `src/fafafa.ssl.wolfssl.context.pas` 里的内嵌 `TWolfSSLConnection`，它虽然有 `SetServerName/GetServerName`，但未声明实现 `ISSLClientConnection`。
  - MbedTLS 真实运行路径走 `src/fafafa.ssl.mbedtls.connection.pas`，同样有方法但未声明实现 `ISSLClientConnection`。
- 这意味着 builder / connector 的 per-connection hostname 语义在这两个 backend 上并不真正可用。
- 在补齐接口契约后，再为连接信息补 native getter：
  - WolfSSL: `wolfSSL_get_servername`
  - MbedTLS: `mbedtls_ssl_get_hostname_pointer`
- 最后修 clear-path parity：
  - WolfSSL 通过 `wolfSSL_set_tlsext_host_name('')` 清空
  - MbedTLS 通过 `mbedtls_ssl_set_hostname(nil)` 清空

**Files**
- Add: `docs/plans/2026-03-09-wolfssl-mbedtls-server-name-observability-parity.md`
- Add: `tests/test_wolfssl_connection_server_name_observability.pas`
- Add: `tests/test_mbedtls_connection_server_name_observability.pas`
- Modify: `src/fafafa.ssl.wolfssl.api.pas`
- Modify: `src/fafafa.ssl.wolfssl.context.pas`
- Modify: `src/fafafa.ssl.mbedtls.api.pas`
- Modify: `src/fafafa.ssl.mbedtls.connection.pas`
- Update: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加两个 backend 的 focused RED。
2. 跑 RED，确认真实连接类缺失 `ISSLClientConnection` 声明。
3. 补接口契约，再跑到下一层 RED。
4. 补 native getter 与 clear-path 实现。
5. 跑 focused suites + compile-all + diff check。
6. 回写 working memory，并给出新的审查建议。

**Expected Outputs**
- WolfSSL / MbedTLS 真实连接对象支持 `ISSLClientConnection`。
- `GetConnectionInfo.ServerName` 能反映 create / override / clear 三个阶段的有效值。
- `SetServerName('')` 在两个 backend 上都不会保留旧的 native hostname。
