# FreePascal Client Chain Trust Runtime Parity Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 pure Pascal TLS 1.3 client 在 `sslVerifyPeer` 下，对 full handshake 的对端证书真正执行 chain trust runtime parity，消费 `SetCertificateStore(...)` / `LoadCAFile(...)` / `LoadCAPath(...)` 的 trust material，并对 untrusted roots fail-closed。

**Architecture:** 这批继续保持 validation hardening 的窄边界：不新造第二套链验证逻辑，而是复用现有 `fafafa.ssl.certchain`，把 context 配置出来的 trusted roots 与 peer `Certificate` flight 自带的 intermediates 分层喂给 verifier，然后再沿用现有的 hostname/expiry helper。实现上只补 FreePascal 私有 context trust-store access、client connection verify wiring，以及 focused scripted tests；不扩到 OCSP/CT、CRL、revocation online fetch、resumed PSK peer-certificate persistence 或 client-side `CertificateVerify` signature verification。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `TFreePascalContext`, `TSSLCertificateChainVerifier`, `ISSLCertificateStore`, TLS 1.3 scripted `TStream` handshake tests, test-only certificate generation, file-based working memory.

---

## Summary

- 当前 FreePascal backend 已经能：
  - 在 full handshake 上要求看到 `Certificate` / `CertificateVerify`
  - 在连接对象上暴露 peer leaf certificate 与 chain
  - 在 runtime 上执行 hostname / expiry parity
- 但 trust material 仍停在 context storage：
  - `SetCertificateStore(...)` 只保存引用
  - `LoadCAFile(...)` / `LoadCAPath(...)` 只保存路径
  - `TFreePascalConnection` 从未把这些配置接到实际握手验证
- 仓库里已经有适合复用的 building blocks：
  - `TSSLCertificateChainVerifier` 支持 trusted/intermediate store 分层
  - FreePascal certificate store 已支持 `LoadFromFile` / `LoadFromPath`
  - peer `Certificate` flight 现在已经能 materialize 成 `FPeerCertificateChain`
- 因此这批只收三条最小 parity：
  - 无 trust material 时，CA-signed full handshake 对端证书在 `sslVerifyPeer` 下 fail-closed
  - `LoadCAFile(...)` / `LoadCAPath(...)` / `SetCertificateStore(...)` 提供的 roots 能驱动握手放行
  - `sslCertVerifyAllowSelfSigned` 能在 full handshake 上有界放宽 self-signed leaf

## Delivery Order

1. 写 plan 与 working-memory 入口，锁定范围为 FreePascal client chain trust runtime parity。
2. 先加 focused RED，证明当前 untrusted / trusted / self-signed cases 仍没有 runtime parity。
3. 最小实现只改 FreePascal 私有 trust-store access 与 client verify wiring。
4. 跑 focused tests、相邻 FreePascal regressions、`python3 scripts/compile_all_modules.py`、diff hygiene。
5. 回填 findings / progress / task plan，并把 queue 收回到 OCSP / CT / remaining validation hardening。

### Task 1: Add RED Coverage For Chain Trust Runtime Parity

**Files:**
- Add: `tests/test_freepascal_client_chain_trust_runtime.pas`
- Reference: `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`

**Step 1: Write the failing scripted full-handshake contracts**
- 新建 `tests/test_freepascal_client_chain_trust_runtime.pas`：
  - 复用 scripted TLS 1.3 client-handshake stream 模式，驱动真实 `TFreePascalConnection.Connect`
  - 生成两类服务端证书：
    - CA-signed leaf：由 `tests/certificate/test_certs/ca_cert.pem` / `ca_key.pem` 签发，SAN=`example.com`
    - self-signed leaf：用 test-only helper 直接生成，SAN=`example.com`
  - 覆盖至少六个 focused cases：
    - default verify flags + CA-signed leaf + no trust material => `Connect = False`
    - default verify flags + `LoadCAFile(ca_cert.pem)` => `Connect = True`
    - default verify flags + `LoadCAPath(<temp dir containing only ca_cert.pem>)` => `Connect = True`
    - default verify flags + `SetCertificateStore(store with ca_cert.pem)` => `Connect = True`
    - default verify flags + self-signed leaf => `Connect = False`
    - `[sslCertVerifyAllowSelfSigned]` + self-signed leaf => `Connect = True`
  - 对失败 case 额外断言：
    - `GetVerifyResult = Ord(sslErrCertificateUntrusted)`
    - `GetVerifyResultString` 包含 `untrusted` 或 `trust`

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime`
- Expected:
  - FAIL because current FreePascal client full handshake still ignores trust-store config and untrusted roots

### Task 2: Implement Minimal Trust-Store Access And Client Verify Wiring

**Files:**
- Modify: `src/fafafa.ssl.freepascal.context.material.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Expose a bounded FreePascal-only trust-store accessor**
- 在 `src/fafafa.ssl.freepascal.context.material.pas` 增加 FreePascal 私有可选接口，供连接层读取 verification trust material。
- 在 `src/fafafa.ssl.freepascal.context.pas` 实现该接口：
  - 基于当前 backend 创建一份 FreePascal certificate store
  - 合并已有 `SetCertificateStore(...)` 传入的 store 内容
  - 再叠加 `LoadCAFile(...)` / `LoadCAPath(...)` 保存的 roots
  - 若最终没有任何 trust material，则返回空结果
- 保持 public `ISSLContext` 接口不变。

**Step 2: Add a bounded client-side chain-trust helper**
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - 新增一个只供 client full-handshake 路径使用的 trust helper
  - 用 peer chain 的 `[1..]` 构造 intermediate store
  - 用 context trust-store accessor 获取 trusted roots
  - 复用 `TSSLCertificateChainVerifier` 验证 leaf
  - 仅在 `sslVerifyPeer in VerifyMode` 且 `not FSessionReused` 时执行
  - verifier fail 时映射到 `sslErrCertificateUntrusted`，错误文本保留 trust reason

**Step 3: Preserve current scope and helper ordering**
- 保持现有 hostname / expiry runtime parity helper 存在，但让它在 trust helper 之后执行。
- 不实现：
  - OCSP stapling / Certificate Transparency
  - CRL / OCSP online revocation runtime parity
  - resumed PSK path 上的 peer-certificate persistence / trust replay
  - client-side `CertificateVerify` signature verification

**Step 4: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Re-Isolate Adjacent Validation Tests And Run Regressions

**Files:**
- Modify if needed: `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Re-isolate the hostname/expiry contract**
- 如果新的 chain-trust parity 让现有 hostname/expiry runtime test 因无 trust roots 而漂移：
  - 在 `tests/test_freepascal_client_cert_verify_flags_runtime.pas` 为相关 context 显式加载 `tests/certificate/test_certs/ca_cert.pem`
  - 保持该文件继续只测试 hostname / expiry，而不重新承担 trust semantics

**Step 2: Re-run focused and adjacent FreePascal regressions**
- Run:
  - `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime`
  - `mkdir -p tmp/freepascal_client_cert_verify_flags_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_cert_verify_flags_runtime -FEtmp/freepascal_client_cert_verify_flags_runtime -otmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime tests/test_freepascal_client_cert_verify_flags_runtime.pas && ./tmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime`
  - `mkdir -p tmp/freepascal_client_peer_certificate_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_peer_certificate_surface -FEtmp/freepascal_client_peer_certificate_surface -otmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface tests/test_freepascal_client_peer_certificate_surface.pas && ./tmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface`
  - `mkdir -p tmp/freepascal_client_certificate_flight_requirements && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_certificate_flight_requirements -FEtmp/freepascal_client_certificate_flight_requirements -otmp/freepascal_client_certificate_flight_requirements/test_freepascal_client_certificate_flight_requirements tests/test_freepascal_client_certificate_flight_requirements.pas && ./tmp/freepascal_client_certificate_flight_requirements/test_freepascal_client_certificate_flight_requirements`
  - `mkdir -p tmp/freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption -FEtmp/freepascal_client_session_resumption -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 3: Diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-09-freepascal-client-chain-trust-runtime-parity.md src/fafafa.ssl.freepascal.context.material.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_chain_trust_runtime.pas tests/test_freepascal_client_cert_verify_flags_runtime.pas task_plan.md findings.md progress.md`
- Expected:
  - exit `0`

### Definition Of Done

- FreePascal client full handshake fails on untrusted CA-signed roots when `sslVerifyPeer` is enabled
- `LoadCAFile(...)` / `LoadCAPath(...)` / `SetCertificateStore(...)` each can supply trust material that allows the same scripted handshake to succeed
- `sslCertVerifyAllowSelfSigned` can boundedly allow self-signed leaf certificates at runtime
- hostname/expiry parity test remains isolated from trust semantics
- focused new regression, adjacent FreePascal regressions, `compile_all_modules.py`, and diff hygiene all pass
- working-memory files record RED/GREEN evidence and the bounded decision to stop before OCSP / CT / revocation / resumed-session trust persistence
