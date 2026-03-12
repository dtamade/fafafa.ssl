# Pure Pascal Client M1 Checklist

这是纯 Pascal / FreePascal 后端在 Linux 上迈向 `HTTPS/TLS 客户端生产可用` 的当前 checklist。

## M1 Target

目标不是“能握一次手就算完成”，而是 Linux-first 的 pure Pascal HTTPS/TLS client 达到可部署、可调试、可持续优化的基线。

M1 验收标准来自总路线图：
- `TLS 1.2 / 1.3`
- 证书链校验
- hostname verification
- 系统根证书
- 自定义 CA / CA bundle
- SNI
- ALPN
- 超时、取消、明确错误语义
- 流式读写与关闭语义
- 日志 / 握手失败原因 / 对端证书信息

## 已满足

当前还没有条目达到“production usable + 证据闭环完整”的完成标准。

这不是否定已有工作，而是刻意保持保守：只有当行为 contract、运行证据、失败语义和用户面文档都闭环后，才会把条目提升到“已满足”。

## 部分满足

| 能力 | 当前状态 | 证据入口 | 说明 |
|---|---|---|---|
| `TLS 1.2 / 1.3` | 部分满足 | `src/fafafa.ssl.freepascal.connection.pas`, `src/fafafa.ssl.freepascal.lib.pas`, `tests/test_freepascal_backend_basic.pas`, `tests/test_freepascal_local_sha384_suite_roundtrip.pas`, `tests/test_freepascal_local_session_resumption_roundtrip.pas`, `tests/scripts/test_freepascal_tls12_local_openssl_contract.sh`, `tests/scripts/test_freepascal_tls12_local_openssl_appdata_contract.sh`, `tests/scripts/test_freepascal_tls12_local_verify_contract.sh`, `tests/scripts/test_freepascal_tls12_shutdown_contract.sh`, `tests/scripts/test_freepascal_tls12_local_openssl_aes_contract.sh`, `tests/scripts/test_freepascal_tls12_local_openssl_aes256_contract.sh`, `tests/scripts/test_freepascal_tls12_local_sigalg_sha512_contract.sh`, `tests/scripts/test_freepascal_tls12_local_sigalg_pss_contract.sh`, `tests/scripts/test_freepascal_tls12_session_surface_contract.sh`, `tests/scripts/test_freepascal_tls12_resumption_truth_matrix_contract.sh`, `tests/scripts/test_freepascal_tls12_resumption_openssl_interop_contract.sh`, `tests/scripts/test_freepascal_tls12_ticket_resumption_openssl_contract.sh`, `tests/scripts/test_freepascal_tls12_builder_connector_resumption_contract.sh`, `tests/scripts/test_freepascal_tls12_builder_connector_ticket_resumption_contract.sh`, `tests/scripts/test_freepascal_tls12_local_p256_contract.sh`, `tests/scripts/test_freepascal_tls12_local_p256_matrix_contract.sh`, `tests/integration/test_freepascal_tls12_system_roots_runtime.pas`, `tests/integration/test_freepascal_tls12_resumption_runtime.pas`, `docs/plans/2026-03-11-pure-pascal-protocol-support-truth.md`, `docs/plans/2026-03-11-pure-pascal-tls12-client-minimum-slice.md`, `docs/plans/2026-03-11-pure-pascal-tls12-sessionid-resumption.md`, `docs/plans/2026-03-11-pure-pascal-tls12-ticket-resumption.md`, `docs/plans/2026-03-11-pure-pascal-tls12-external-resumption-matrix.md` | 当前 pure Pascal 主线仍默认以 TLS 1.3 为推荐真相（default config/context 继续 `TLS1.3-only`），但 TLS 1.2 已经从 unsupported 演进到一条明确受限的 client path：显式 opt-in 下，当前可完成本地 OpenSSL `TLS1.2` 的 handshake、app-data、verify-on、shutdown，以及 `session-id + ticket` 两类 resumption。当前本地互操作已覆盖 `TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256`、`TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256`、`TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384`，`ServerKeyExchange` 签名已覆盖 `rsa_pkcs1_sha256` / `rsa_pkcs1_sha512` / `rsa_pss_rsae_sha256`，`P-256` ECDHE 也已在本地矩阵下通过。另在 `2026-03-11` 的 network-gated runtime probe 中，`www.apache.org`、`www.perl.org`、`ftp.gnu.org`、`www.debian.org`、`www.kernel.org`、`www.gnu.org` 已通过 TLS1.2 resumption strict probe，`rsa2048.badssl.com` 也已在严格模式下复用成功。TLS 1.2 `GetSession/SetSession` 当前已经不只是 surface：低层与高层入口都已有 resumed path 证据，但更广外部 interop、更多 runtime/service 矩阵和更完整的生产级 coverage 仍待补齐，因此离 “1.2 / 1.3 都稳定握手” 还有差距。 |
| 证书链校验 | 部分满足 | `docs/plans/2026-02-12-repo-gap-iteration-98-backend-contract-freepascal-parity.md`, `tests/test_freepascal_backend_basic.pas`, `tests/test_freepascal_client_chain_verification_path.pas`, `tests/test_freepascal_client_custom_ca_sources_path.pas` | pure Pascal 客户端现在不再只看 leaf；验证路径会消费 peer chain intermediates，并沿 issuer 链走到 trusted root。这样已经足以覆盖 `leaf + intermediate + trusted root` 的真实握手合同，但仍未达到完整 PKIX/签名级严格验证。 |
| hostname verification | 部分满足 | `src/fafafa.ssl.freepascal.connection.pas`, `tests/test_freepascal_client_hostname_verification_path.pas`, `docs/plans/2026-03-10-pure-pascal-client-hostname-verification-path.md`, `tests/integration/test_freepascal_system_roots_runtime.pas` | FreePascal backend 的 SAN/hostname 语义不再只停留在 cert-object parity；当前客户端 `Connect` 路径已在 `sslVerifyPeer` 下执行 hostname verification，并对 mismatch 显式失败。现在 system-roots 真实外网握手已扩到小型多站点 matrix，但更广的站点/平台矩阵仍待补齐。 |
| 系统根证书 | 部分满足 | `docs/plans/2026-02-11-repo-gap-iteration-12-freepascal-capability-systemstore.md`, `tests/integration/test_freepascal_system_roots_runtime.pas`, `docs/plans/2026-03-10-pure-pascal-client-system-roots-runtime.md`, `docs/plans/2026-03-11-pure-pascal-system-roots-runtime-matrix.md` | builder `WithSystemRoots` 现在已有 env-gated 的真实外网 TLS 1.3 小型矩阵证据；当前 integration 默认会保守探测 `www.google.com`、`www.cloudflare.com`、`www.github.com`，也支持 `FAFAFA_SYSTEM_ROOTS_HOSTS` / `FAFAFA_SYSTEM_ROOTS_HOST` 做受控覆盖。但它仍然是 network-gated、Linux-first 的证据，不是完整的多平台生产矩阵。 |
| 自定义 CA / CA bundle | 部分满足 | `src/fafafa.ssl.freepascal.connection.pas`, `tests/test_freepascal_client_custom_ca_sources_path.pas`, `tests/test_freepascal_client_trust_source_runtime_matrix.pas`, `docs/plans/2026-03-10-pure-pascal-client-custom-ca-sources-path.md` | pure Pascal 客户端现在已经不只是在 scripted 握手上消费 `LoadCAFile(...)` / `LoadCAPath(...)`；本地真实 socket runtime 也已覆盖 `SetCertificateStore` / `LoadCAFile` / `LoadCAPath` 三条正路径和 “无信任源失败” 负路径。但 system-roots 的更强运行矩阵、以及更广环境证据仍待补齐。 |
| 密码保护私钥 | 部分满足 | `tests/test_freepascal_password_callback_runtime_path.pas`, `tests/test_freepascal_password_protected_private_key_truth.pas`, `docs/plans/2026-03-10-pure-pascal-password-protected-private-key-runtime.md` | pure Pascal backend 现在已有真实运行时支持：`LoadPrivateKey*` 的密码字符串路径与 `SetPasswordCallback(...)` 都能把加密私钥走到 server signer 主线。当前支持范围包括 `PBES2 + PBKDF2-HMAC-SHA256 + AES-CBC`，以及 legacy AES-CBC PEM；但非 AES 的 legacy PEM cipher family 仍未覆盖。 |
| SNI | 部分满足 | `src/fafafa.ssl.freepascal.connection.pas`, `docs/plans/2026-03-09-server-name-migration-policy.md`, `docs/plans/2026-03-current-summary.md` | clienthello 路径已带 `FServerName`，ServerName contract 也已收口，但 pure Pascal HTTPS client 端到端生产证据仍不足。 |
| ALPN | 部分满足 | `src/fafafa.ssl.freepascal.connection.pas`, `tests/test_tls13_clienthello_parser.pas`, `tests/test_tls13_encrypted_extensions_parser.pas`, `tests/test_freepascal_server_accept_skeleton.pas`, `tests/test_freepascal_client_peer_certificate_foundation.pas`, `tests/scripts/test_freepascal_tls13_builder_connector_stream_alpn_contract.sh`, `tests/scripts/test_freepascal_tls12_local_alpn_openssl_contract.sh`, `tests/scripts/test_freepascal_tls13_builder_connector_stream_alpn_http11_fallback_contract.sh`, `tests/scripts/test_freepascal_tls12_local_alpn_http11_fallback_contract.sh`, `tests/scripts/test_freepascal_tls13_builder_connector_stream_no_alpn_contract.sh`, `tests/scripts/test_freepascal_tls12_local_no_alpn_contract.sh`, `tests/scripts/test_freepascal_tls13_builder_connector_stream_client_no_alpn_offer_contract.sh`, `tests/scripts/test_freepascal_tls12_local_client_no_alpn_offer_contract.sh`, `tests/integration/test_freepascal_alpn_runtime.pas`, `docs/plans/2026-03-11-pure-pascal-alpn-runtime-evidence.md`, `docs/plans/2026-03-11-pure-pascal-alpn-fallback-matrix.md`, `docs/plans/2026-03-11-pure-pascal-alpn-empty-negotiation.md`, `docs/plans/2026-03-11-pure-pascal-alpn-client-no-offer.md` | 纯 Pascal 路径现在不只具备 ALPN offer、server selection、EncryptedExtensions / ServerHello parse 与 observability；本地 OpenSSL 真实互操作已覆盖 TLS1.3 主入口 `WithHTTP2`、TLS1.2 client path、`http/1.1` fallback、“服务端不发送 ALPN 扩展”时的空协商语义，以及“客户端不发送 ALPN 扩展”时的空协商语义。network-gated runtime 也已在 `www.cloudflare.com`、`www.github.com` 上同时锁定过 `h2` 与 `http/1.1` 两组期望协议，并支持后续显式允许空协商的 harness 结构。但更广平台矩阵、更多服务类型和更完整的生产级 runtime coverage 仍待补齐，所以当前仍保守保持“部分满足”。 |
| 超时、取消、明确错误语义 | 部分满足 | `docs/plans/2026-02-11-repo-gap-iteration-2-freepascal-precondition.md`, `docs/plans/2026-02-11-repo-gap-iteration-3-freepascal-unsupported-semantics.md`, `tests/scripts/test_freepascal_blocking_read_timeout_contract.sh`, `tests/scripts/test_freepascal_blocking_write_timeout_contract.sh`, `tests/scripts/test_freepascal_client_handshake_timeout_contract.sh` | precondition/unsupported error semantics 已经更清楚，blocking `SetTimeout(...)` 在 read / write / client handshake 三条路径上也已有 `sslErrTimeout` 的真实 contract。但 cancel surface 仍未形成独立 client M1 contract。 |
| 流式读写与关闭语义 | 部分满足 | `src/fafafa.ssl.freepascal.connection.pas`, `tests/scripts/test_freepascal_stream_semantics_contract.sh`, `tests/scripts/test_freepascal_nonblocking_partial_record_contract.sh`, `tests/scripts/test_freepascal_nonblocking_write_wantwrite_contract.sh`, `tests/scripts/test_freepascal_shutdown_nonblocking_retry_contract.sh`, `tests/scripts/test_freepascal_retry_success_clears_stale_error_contract.sh` | 当前已具备：真实 socket-level `SetBlocking(False)`、`WantRead/WantWrite`、split-record continuation、`close_notify` 主动/被动 graceful close、pending-write 下的 `Shutdown` retry，以及 retry 成功后的干净 detail。它已经明显强于“只有基础 stream/socket 双路径”，但还没覆盖取消语义、更广环境矩阵和更完整的 post-handshake 行为矩阵，因此当前仍保守定为“部分满足”。 |
| 日志 / 握手失败原因 / 对端证书信息 | 部分满足 | `src/fafafa.ssl.freepascal.connection.pas`, `tests/test_freepascal_client_peer_certificate_foundation.pas`, `tests/test_freepascal_client_hostname_verification_path.pas`, `tests/test_freepascal_verify_result_string_observability.pas`, `tests/scripts/test_freepascal_handshake_failed_state_contract.sh`, `docs/plans/2026-03-10-pure-pascal-client-peer-certificate-foundation.md` | 已有更清晰的握手失败原因、selected ALPN、scripted client handshake 后可取到的对端 leaf/chain 信息，以及 hostname mismatch 的显式 verify 错误。现在 `GetVerifyResultString` 也能区分“验证通过/验证关闭”，`GetState/GetStateString` 也不再把握手失败误报成 `Disconnected`。但更完整的日志/事件矩阵和 broader runtime coverage 仍待补齐。 |

## 缺失

- 当前没有单独条目仍处于“完全缺失”。
- 但这不代表 M1 已完成；上表的大部分能力仍只有“部分满足”，还缺生产级验证闭环。

## 结论

当前纯 Pascal client M1 的总体判断是：
- **不是从零开始**
- 但也**还不能宣称生产可用**

最合理的下一步不是继续零散补小点，而是把上表中的“部分满足 / 缺失”拆成 focused contracts，逐条转成明确的：
- 已满足
- 部分满足
- 缺失

这样进入实现期后，纯 Pascal 后端才能有稳定的 M1 收口路径。
