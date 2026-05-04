# WolfSSL Client Peer Certificate Chain Surface Plan

**Goal:** 让 WolfSSL client connection 在成功的 full handshake 后，真正暴露 peer leaf certificate 与 peer certificate chain，而不再让 `GetPeerCertificateChain` 永远返回空数组。

**Architecture:** 这批优先修 core verification surface，而不是继续做 docs-only 或 capability-only closeout。最初确实尝试复用 scripted TLS 1.3 server harness，想用真实 full handshake 打出 RED；但在当前主机 `wolfSSL 5.7.2` 上，这条路径只得到 `Connect=False / verify=OK` 之类不可信号，不能拿来当最终收口证据。最终方案因此收窄成 deterministic native-surface contract：用真实 DER fixture 锁住 `wolfSSL_X509_d2i` 导入路径，再覆盖 `wolfSSL_get_peer_chain` / `wolfSSL_get_chain_count` / `wolfSSL_get_chain_length` / `wolfSSL_get_chain_cert`，验证 `TWolfSSLConnection.DoGetPeerCertificateChain` 会把 native chain entry materialize 成 `ISSLCertificate` 数组，同时在 helper 缺失时 fail-closed 为现有空数组 contract。验证阶段额外暴露了 `Renegotiate` 的静默失败漂移，所以顺手把它收成显式 unsupported 语义，避免 framework test 继续虚绿。

**Files:**
- Add: `tests/connection/test_wolfssl_client_peer_certificate_surface.pas`
- Modify: `src/fafafa.ssl.wolfssl.base.pas`
- Modify: `src/fafafa.ssl.wolfssl.api.pas`
- Modify: `src/fafafa.ssl.wolfssl.connection.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - probe the runtime path, then narrow to a trustworthy proof

Run:

```bash
fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_wolfssl_client_peer_certificate_surface tests/connection/test_wolfssl_client_peer_certificate_surface.pas
./tmp/test_wolfssl_client_peer_certificate_surface
```

Expected:
- 如果 WolfSSL runtime 不可用，测试必须结构化 `SKIP`
- 如果 runtime path 给出稳定 full-handshake 结果，就直接暴露 `GetPeerCertificateChain` 的 RED
- 如果 runtime path 只给出像当前 host `5.7.2` 这种 `Connect=False / verify=OK` 的不可信号，就必须收窄成 deterministic contract，而不是把模糊 runtime 行为包装成完成证据

## Task 2: GREEN - bind and materialize the peer chain

Change:
- `src/fafafa.ssl.wolfssl.base.pas`
  - 修正 `wolfSSL_get_error()` 常量取值
  - 增加 peer-chain opaque pointer type
- `src/fafafa.ssl.wolfssl.api.pas`
  - 把 `TwolfSSL_X509_d2i` 绑定到真实的 `wolfSSL_X509_d2i`
  - 绑定 `wolfSSL_get_peer_chain`
  - 绑定 `wolfSSL_get_chain_count`
  - 绑定 `wolfSSL_get_chain_length`
  - 绑定 `wolfSSL_get_chain_cert`
- `src/fafafa.ssl.wolfssl.connection.pas`
  - 用 wolfSSL peer-chain APIs 拉取 DER bytes
  - 把每个证书 materialize 成 `ISSLCertificate`
  - 保持 helper 缺失时 safe degrade 为现有空数组 contract
  - 把 `Renegotiate` 的静默失败收成显式 unsupported 语义

Constraints:
- 不改 WolfSSL 握手流程
- 不直接接管 wolfSSL peer-chain X509 的 native ownership
- 不把这批扩大到 trust-store / hostname / OCSP / CT 主线

## Task 3: Verification

Run:

```bash
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/wolfssl_peer_chain_surface_units -FEtmp/wolfssl_peer_chain_surface_units -otmp/wolfssl_peer_chain_surface_units/test_wolfssl_client_peer_certificate_surface tests/connection/test_wolfssl_client_peer_certificate_surface.pas
./tmp/wolfssl_peer_chain_surface_units/test_wolfssl_client_peer_certificate_surface
fpc -B -Fu./src -Fu./tests -FUtmp/wolfssl_framework_units -FEtmp/wolfssl_framework_units -otmp/wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas
./tmp/wolfssl_framework_units/test_wolfssl_framework
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- WolfSSL peer-chain surface 会从 native helper 拉取 DER 并 materialize 成 `ISSLCertificate` 数组
- `GetPeerCertificateChain` 不再是无条件空实现
- `LoadFromDER(...)` 不再因为错误的 `wolfSSL_X509_d2i` 绑定触发 AV
- `Renegotiate` 不再是静默 `False`，而会给出显式 unsupported 诊断
- focused contract、`test_wolfssl_framework`、compile gate、minimal CI gate 全绿
- 当前 host `wolfSSL 5.7.2` runtime 限制已写回台账，避免后续重复从不可信 runtime 路径继续空转
- 台账同步到新的 WolfSSL verification truth
