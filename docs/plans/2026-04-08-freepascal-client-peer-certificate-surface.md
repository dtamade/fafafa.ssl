# FreePascal Client Peer Certificate Surface Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 pure Pascal TLS 1.3 client 在成功的 full handshake 后，能够从连接对象暴露对端 leaf certificate 与 peer certificate chain，而不再一律返回 `nil` / 空数组。

**Architecture:** 这批继续保持 validation hardening 的窄边界：只补 `Certificate` 消息解析与 connection-level peer-certificate surface，不把 scope 扩到 hostname / expiry runtime parity、完整链验证、`CertificateVerify` signature verification、OCSP/CT，或 session persistence 对 peer certificate 的持久化。实现策略是先用一个 scripted full-handshake RED 证明当前 `GetPeerCertificate` / `GetPeerCertificateChain` 仍是空，再在 `fafafa.ssl.tls13.servercertificate` 增加最小 parser，并在 `TFreePascalConnection.ProcessEncryptedServerFlight(...)` 收到 `Certificate` 时加载并缓存 `ISSLCertificate` 对象。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, TLS 1.3 handshake helpers, `fafafa.ssl.tls13.servercertificate`, `TSSLFactory.CreateCertificate(sslFreePascal)`, offline scripted `TStream` tests, file-based working memory.

---

## Summary

- 当前 FreePascal client 已经能在握手 transcript 里吞掉 `Certificate` 消息，但 connection surface 还没有任何 peer-certificate 真值：
  - `DoGetPeerCertificate` 直接返回 `nil`
  - `DoGetPeerCertificateChain` 直接返回空数组
- 仓库里已经有可复用的纯 Pascal building blocks：
  - `fafafa.ssl.tls13.servercertificate` 能把 PEM/DER blob 构造成 TLS 1.3 `Certificate` handshake
  - `ISSLCertificate.LoadFromDER(...)` 已经在 FreePascal backend 可用
- 因此这批只做三件事：
  - 给 TLS 1.3 `Certificate` handshake 增加最小 parser
  - 在 client full handshake 上缓存 leaf + chain
  - 用 focused scripted test 锁定 surface contract

## Delivery Order

1. 写 plan 与 working-memory 入口，锁定范围为 FreePascal client peer-certificate surface。
2. 先加 focused RED，证明 full handshake 已经成功时 `GetPeerCertificate` / `GetPeerCertificateChain` 仍是空。
3. 最小实现只改 `src/fafafa.ssl.tls13.servercertificate.pas` 与 `src/fafafa.ssl.freepascal.connection.pas`。
4. 跑 focused tests、相邻 FreePascal regressions、`python3 scripts/compile_all_modules.py`、diff hygiene。
5. 回填 findings / progress / task plan，并把 queue 继续收回到 hostname / expiry runtime parity。

### Task 1: Add RED Coverage For Peer Certificate Surface

**Files:**
- Add: `tests/test_freepascal_client_peer_certificate_surface.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`
- Reference: `src/fafafa.ssl.tls13.servercertificate.pas`
- Reference: `tests/test_freepascal_client_certificate_flight_requirements.pas`

**Step 1: Write the failing scripted full-handshake contract**
- 新建 `tests/test_freepascal_client_peer_certificate_surface.pas`：
  - 复用 scripted client-handshake stream 模式，驱动真实 `TFreePascalConnection.Connect`
  - 服务端脚本发送：
    - `ServerHello`
    - `EncryptedExtensions`
    - `Certificate`（leaf=`signer_cert.pem`，chain 带 `ca_cert.pem`）
    - `CertificateVerify`
    - `Finished`
  - 断言：
    - `Connect = True`
    - `GetPeerCertificate <> nil`
    - `GetPeerCertificateChain` 长度至少为 `2`
    - leaf fingerprint / subject 与 fixture 一致
    - chain 尾部 issuer subject 与 `ca_cert.pem` 一致
- client context 显式 `SetVerifyMode([])`，把测试范围收窄到 certificate surface，不把当前 validation parity 缺口写进 contract。

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_client_peer_certificate_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_peer_certificate_surface -FEtmp/freepascal_client_peer_certificate_surface -otmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface tests/test_freepascal_client_peer_certificate_surface.pas && ./tmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface`
- Expected:
  - FAIL because current FreePascal client path still returns `nil` / empty chain even after a successful full handshake with a `Certificate` message

### Task 2: Implement Minimal Client Peer Certificate Parsing And Surface

**Files:**
- Modify: `src/fafafa.ssl.tls13.servercertificate.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add TLS 1.3 Certificate handshake parser**
- 在 `src/fafafa.ssl.tls13.servercertificate.pas`：
  - 增加 parser，把 TLS 1.3 `Certificate` handshake 解成 DER certificate array
  - 至少校验：
    - handshake type 必须是 `TLS_HANDSHAKE_TYPE_CERTIFICATE`
    - body 长度 / `certificate_request_context` / `certificate_list` 长度一致
    - 每个 entry 的 DER 长度与 extension 长度合法

**Step 2: Cache peer certificate objects on the connection**
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - 增加 connection-level peer certificate fields
  - 在 client `ProcessEncryptedServerFlight(...)` 收到 `TLS_HANDSHAKE_TYPE_CERTIFICATE` 时：
    - 调用新 parser 提取 DER list
    - 用 `TSSLFactory.CreateCertificate(sslFreePascal)` + `LoadFromDER(...)` 构造 `ISSLCertificate`
    - 缓存 leaf 与 chain
  - 在新的 `Connect` 开始、`Close`、失败路径上清理缓存，避免旧连接状态泄漏
  - `DoGetPeerCertificate` / `DoGetPeerCertificateChain` 返回缓存值

**Step 3: Preserve current validation boundary**
- 不实现：
  - hostname / expiry runtime parity
  - chain trust verification
  - `CertificateVerify` signature verification
  - resumed PSK 通过 session persistence 继承 peer certificate
- 如果 `Certificate` handshake 自身结构损坏或 DER 无法加载，则当前握手 fail-closed。

**Step 4: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Run Focused Regressions And Gate

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Re-run focused and adjacent FreePascal regressions**
- Run:
  - `mkdir -p tmp/freepascal_client_peer_certificate_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_peer_certificate_surface -FEtmp/freepascal_client_peer_certificate_surface -otmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface tests/test_freepascal_client_peer_certificate_surface.pas && ./tmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface`
  - `mkdir -p tmp/freepascal_client_certificate_flight_requirements && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_certificate_flight_requirements -FEtmp/freepascal_client_certificate_flight_requirements -otmp/freepascal_client_certificate_flight_requirements/test_freepascal_client_certificate_flight_requirements tests/test_freepascal_client_certificate_flight_requirements.pas && ./tmp/freepascal_client_certificate_flight_requirements/test_freepascal_client_certificate_flight_requirements`
  - `mkdir -p tmp/freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption -FEtmp/freepascal_client_session_resumption -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 2: Diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-08-freepascal-client-peer-certificate-surface.md src/fafafa.ssl.tls13.servercertificate.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_peer_certificate_surface.pas task_plan.md findings.md progress.md`
- Expected:
  - exit `0`

### Definition Of Done

- successful FreePascal client full handshake can expose leaf peer certificate via `GetPeerCertificate`
- successful FreePascal client full handshake can expose peer chain via `GetPeerCertificateChain`
- malformed `Certificate` handshake / unparseable DER no longer silently degrades to `nil`
- focused new regression, adjacent FreePascal regressions, `compile_all_modules.py`, and diff hygiene all pass
- working-memory files record RED/GREEN evidence and the bounded decision to stop before hostname / expiry runtime parity
