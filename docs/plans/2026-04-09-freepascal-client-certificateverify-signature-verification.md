# FreePascal Client CertificateVerify Signature Verification Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 pure Pascal TLS 1.3 client 在 `sslVerifyPeer` 的 full handshake 路径上，真正校验服务端 `CertificateVerify` 签名，并对坏签名 / 错 scheme / 不匹配公钥 fail-closed。

**Architecture:** 这批继续保持 validation hardening 的窄边界：不扩到 CT、OCSP online fetch、完整 policy 叠加或 capability wording，只补 `CertificateVerify` 这条 runtime fail-closed 缺口。实现路径是：先在 scripted full-handshake contract 上制造可控坏签名，证明当前 client 仍错误放行；然后在 `fafafa.ssl.tls13.servercertverify` 增加 bounded parser + pure Pascal public-key verify helper（SHA-256 / RSA-PSS / RSA-PKCS1v1.5 / ECDSA P-256），最后把校验接到 `TFreePascalConnection.ProcessEncryptedServerFlight(...)` 的 `CertificateVerify` 分支。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `fafafa.ssl.tls13.servercertverify`, `fafafa.ssl.tls13.ecdsa`, `fafafa.ssl.tls13.bigint`, `TX509Certificate.PublicKeyInfo`, TLS 1.3 scripted `TStream` handshake tests, file-based working memory.

---

## Summary

- 当前 FreePascal client validation hardening 已经补到了：
  - full handshake 必须看到 `Certificate` / `CertificateVerify`
  - peer certificate surface
  - hostname / expiry runtime parity
  - chain trust parity
  - stapled OCSP bounded surface / required policy
- 但还有一个核心 fail-open 缺口：
  - `ProcessEncryptedServerFlight(...)` 在 client path 上虽然要求必须收到 `CertificateVerify`
  - 却从未校验该消息的 signature scheme / signature bytes 是否真能被 leaf certificate 公钥验证
  - 这意味着服务端只要发了一个格式合法但内容错误的 `CertificateVerify`，当前 client 仍可能继续通过
- 仓库里已有足够的纯 Pascal building blocks 支撑这批做成一个窄而真实的 batch：
  - `fafafa.ssl.tls13.servercertverify` 已有 TLS 1.3 `CertificateVerify` input builder 与 RSA/ECDSA signer 逻辑
  - `fafafa.ssl.tls13.bigint` 已有 RSA 模幂能力
  - `fafafa.ssl.tls13.ecdsa` 已有 P-256 点运算 / 模逆 / signer 所需数学
  - `TX509Certificate.PublicKeyInfo` 已能暴露 RSA modulus/exponent 与 ECDSA ECPoint
- 这批刻意不做：
  - CT / SCT policy
  - OCSP online fetch 或更强 revocation parity
  - `Certificate` chain/path policy 新能力
  - capability wording 调整

## Delivery Order

1. 写 plan 与 working-memory 入口，锁定范围为 FreePascal client `CertificateVerify` signature verification。
2. 先加 focused RED，证明当前 bad `CertificateVerify` 仍会被错误接受。
3. 最小实现只补 `tls13.servercertverify`、`tls13.ecdsa` 与 `freepascal.connection`。
4. 跑 focused tests、相邻 FreePascal regressions、`python3 scripts/compile_all_modules.py`、diff hygiene。
5. 回填 findings / progress / task plan，并把 queue 继续收回到 CT / remaining validation hardening。

### Task 1: Add RED Coverage For Client CertificateVerify Runtime

**Files:**
- Add: `tests/test_freepascal_client_certificateverify_runtime.pas`
- Reference: `tests/test_freepascal_client_chain_trust_runtime.pas`
- Reference: `tests/test_freepascal_client_peer_certificate_surface.pas`
- Reference: `tests/test_freepascal_client_ocsp_stapling_runtime.pas`

**Step 1: Write the failing scripted full-handshake contract**
- 新建 `tests/test_freepascal_client_certificateverify_runtime.pas`：
  - 复用 scripted TLS 1.3 client-handshake stream，驱动真实 `TFreePascalConnection.Connect`
  - client context 显式 `SetVerifyMode([sslVerifyPeer])` 并加载 `tests/certificate/test_certs/ca_cert.pem`，把测试范围收窄到 `CertificateVerify` 而不是 trust gap
  - 服务端脚本至少覆盖以下 cases：
    - CA-signed RSA leaf + valid `CertificateVerify` => `Connect = True`
    - 同一 handshake 但 `CertificateVerify` signature 尾字节被翻转 => `Connect = False`
    - 同一 handshake 但 `CertificateVerify` scheme 与 leaf key type 不匹配 => `Connect = False`
  - 如果纯 Pascal ECDSA leaf fixture 构造成本可控，再额外覆盖：
    - ECDSA P-256 leaf + valid `CertificateVerify` => `Connect = True`
    - ECDSA P-256 leaf + tampered `CertificateVerify` => `Connect = False`

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_client_certificateverify_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_certificateverify_runtime -FEtmp/freepascal_client_certificateverify_runtime -otmp/freepascal_client_certificateverify_runtime/test_freepascal_client_certificateverify_runtime tests/test_freepascal_client_certificateverify_runtime.pas && ./tmp/freepascal_client_certificateverify_runtime/test_freepascal_client_certificateverify_runtime`
- Expected:
  - FAIL because current FreePascal client still accepts malformed or mismatched `CertificateVerify`

### Task 2: Implement Minimal Parser / Public-Key Verify / Handshake Wiring

**Files:**
- Modify: `src/fafafa.ssl.tls13.servercertverify.pas`
- Modify: `src/fafafa.ssl.tls13.ecdsa.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add bounded `CertificateVerify` parsing**
- 在 `src/fafafa.ssl.tls13.servercertverify.pas`：
  - 增加 `TryParseTLS13CertificateVerifyHandshake(...)`
  - 至少校验：
    - handshake type 必须是 `TLS_HANDSHAKE_TYPE_CERTIFICATE_VERIFY`
    - body 长度一致
    - `signature_scheme` 已知且长度字段合法
    - signature bytes 非空

**Step 2: Add pure Pascal public-key verify helpers**
- 在 `src/fafafa.ssl.tls13.servercertverify.pas`：
  - 增加基于 `TX509PublicKeyInfo` 的 verify helper
  - RSA 路径：
    - 对 signature 做 public exponent 模幂还原 encoded message
    - 校验 `RSA-PSS-SHA256`
    - 校验 `RSA-PKCS1v1.5-SHA256`
  - ECDSA 路径：
    - 只支持 `ECDSA_secp256r1_sha256`
    - 把 verify 数学下沉到 `src/fafafa.ssl.tls13.ecdsa.pas`
- 在 `src/fafafa.ssl.tls13.ecdsa.pas`：
  - 增加最小 `TryECDSAVerifyP256SHA256(...)`
  - 复用现有 P-256 point ops / modular arithmetic
  - 支持 DER `SEQUENCE(INTEGER r, INTEGER s)` 解析与 low-level range checks

**Step 3: Wire verification into the successful client handshake path**
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - 增加一个只供 client full-handshake 路径使用的 helper，例如 `ValidateServerCertificateVerify(...)`
  - helper 读取：
    - `FPeerCertificate`
    - 当前 `CertificateVerify` handshake bytes
    - `ACipherSuite`
    - `ATranscriptData`（注意必须使用 `CertificateVerify` 之前的 transcript）
  - 行为：
    - 仅在 `sslVerifyPeer in VerifyMode` 且 `not FSessionReused` 时执行
    - 缺少 peer certificate => fail-closed 为 `sslErrCertificate`
    - parser / scheme / public-key verify 任一步失败 => fail-closed 为 `sslErrHandshake` 或 `sslErrProtocol`
  - 保持 transcript append 顺序正确：
    - 先用当前 transcript 计算 verify input
    - 验签成功后再把 `CertificateVerify` append 进 transcript

**Step 4: Preserve scope honesty**
- 不实现：
  - CT / SCT policy enforcement
  - OCSP / CRL / online revocation checks
  - 更宽的 ECDSA 曲线支持（只做当前已有 `prime256v1` / P-256）
  - SHA384 / SHA512 `CertificateVerify` 扩展，除非测试明确要求
- 如果 peer leaf key type 超出当前 bounded verifier 能力：
  - 在 verify-required 的 full-handshake client path 上 fail-closed 为 unsupported/handshake error
  - 不伪装成“已验证”

**Step 5: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Re-Run Adjacent Regressions And Gate

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run focused and adjacent regressions**
- Run:
  - `mkdir -p tmp/freepascal_client_certificateverify_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_certificateverify_runtime -FEtmp/freepascal_client_certificateverify_runtime -otmp/freepascal_client_certificateverify_runtime/test_freepascal_client_certificateverify_runtime tests/test_freepascal_client_certificateverify_runtime.pas && ./tmp/freepascal_client_certificateverify_runtime/test_freepascal_client_certificateverify_runtime`
  - `mkdir -p tmp/freepascal_client_peer_certificate_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_peer_certificate_surface -FEtmp/freepascal_client_peer_certificate_surface -otmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface tests/test_freepascal_client_peer_certificate_surface.pas && ./tmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface`
  - `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime`
  - `mkdir -p tmp/freepascal_client_cert_verify_flags_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_cert_verify_flags_runtime -FEtmp/freepascal_client_cert_verify_flags_runtime -otmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime tests/test_freepascal_client_cert_verify_flags_runtime.pas && ./tmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime`
  - `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime`
  - `mkdir -p tmp/freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption -FEtmp/freepascal_client_session_resumption -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`

**Step 2: Run module gate and diff hygiene**
- Run:
  - `python3 scripts/compile_all_modules.py`
  - `git diff --check -- docs/plans/2026-04-09-freepascal-client-certificateverify-signature-verification.md src/fafafa.ssl.tls13.servercertverify.pas src/fafafa.ssl.tls13.ecdsa.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_certificateverify_runtime.pas task_plan.md findings.md progress.md`

**Step 3: Update ledgers**
- 在 `task_plan.md`、`findings.md`、`progress.md` 记录：
  - RED/GREEN 证据
  - 当前 bounded verifier 支持范围
  - 明确保留 CT / OCSP / 更广 curve/hash 支持在本批之外

### Definition Of Done

- FreePascal client full-handshake path 不再只检查 `CertificateVerify` 的“存在性”，而是会实际校验其签名
- malformed / mismatched / tampered `CertificateVerify` 会 fail-closed
- focused runtime contract、相邻 FreePascal regressions、`compile_all_modules.py`、diff hygiene 全绿
- working-memory 文件明确记录本批边界：不是 CT，也不是完整证书验证终局
