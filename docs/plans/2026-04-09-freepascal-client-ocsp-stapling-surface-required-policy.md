# FreePascal Client OCSP Stapling Surface And Required Policy Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 pure Pascal TLS 1.3 client 在 `ssoEnableOCSPStapling` 下真正发送 `status_request`，从服务端 `Certificate` 里的 stapled OCSP response 提取并暴露 `ISSLOCSPStapling` surface，并在 `ssoRequireOCSPStapling` 下对缺失或未通过有界校验的 stapled response fail-closed。

**Architecture:** 这批继续保持 validation hardening 的窄边界，只做 FreePascal client-side runtime parity 的一个有界子集，不把能力说大成“完整 revocation parity”。实现路径是：在 TLS 1.3 ClientHello builder 中按 context option 追加 `status_request` 扩展；在 TLS 1.3 `Certificate` parser 中暴露 leaf `CertificateEntry` 上的 `status_request` extension；在 `TFreePascalConnection` 缓存 stapled response、基于现有 `TOCSPStaplingClient` 做 DER 解析 / freshness / CertID 匹配 / required-policy 判定，并把结果映射到 `ISSLOCSPStapling`。明确不扩到 online AIA OCSP fetch、responder-signature/chain cryptographic verification、server-side stapling issuance、CT，且暂不修改 FreePascal capability wording。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `fafafa.ssl.tls13.clienthello`, `fafafa.ssl.tls13.servercertificate`, `TOCSPStaplingClient`, TLS 1.3 scripted `TStream` handshake tests, OCSP DER fixtures, file-based working memory.

---

## Summary

- 当前 FreePascal client 在 OCSP stapling 这条链上还停在三个断点：
  - ClientHello 不会发送 `status_request`
  - TLS 1.3 `Certificate` parser 会丢弃 `CertificateEntry.extensions`
  - connection surface 仍回退到 `TBaseSSLConnection` 默认桩实现
- 这批只补一个诚实、可验证的子集：
  - `ssoEnableOCSPStapling` 时客户端确实请求 stapling
  - full handshake 成功后可以通过 `ISSLOCSPStapling` 读到 stapled response / verification bit / status text
  - `ssoRequireOCSPStapling` 时，缺失 response 或 response 未通过当前有界校验都会 fail-closed
- 明确保留在本批之外：
  - `sslCertVerifyCheckOCSP` 的 online OCSP fetch parity
  - responder signature / trust-chain cryptographic verification
  - server-side stapling issuance
  - capability 文案从 “remaining gap” 改成 “supported”

## Delivery Order

1. 写 plan 与 working-memory 入口，锁定范围为 FreePascal client OCSP stapling surface + required-policy。
2. 先加 focused RED，证明当前 ClientHello 仍不发 `status_request`，且 `ssoRequireOCSPStapling` 还不会 fail-closed。
3. 最小实现只改 TLS 1.3 wire/clienthello、server certificate parser、FreePascal connection。
4. 跑 focused tests、相邻 FreePascal regressions、`python3 scripts/compile_all_modules.py`、diff hygiene。
5. 回填 findings / progress / task plan，并把 queue 继续收回到 CT / remaining validation hardening。

### Task 1: Add RED Coverage For Client OCSP Stapling Runtime

**Files:**
- Add: `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- Reference: `tests/test_freepascal_client_chain_trust_runtime.pas`
- Reference: `tests/openssl/test_ocsp_connection_verification_regression.pas`
- Reference: `tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der`
- Reference: `tests/fixtures/p2/ocsp/ocsp_response_malformed_v1.der`

**Step 1: Write the failing scripted full-handshake contract**
- 新建 `tests/test_freepascal_client_ocsp_stapling_runtime.pas`：
  - 复用 scripted TLS 1.3 client-handshake stream，驱动真实 `TFreePascalConnection.Connect`
  - 服务端脚本使用 CA-signed leaf + CA chain，并让 client 显式加载 `tests/certificate/test_certs/ca_cert.pem`，把测试范围收窄到 OCSP stapling 而不是 trust semantics
  - 在 test-local helper 里手工构造带 leaf `status_request` extension 的 TLS 1.3 `Certificate` handshake
  - 覆盖至少四个 focused cases：
    - `ssoEnableOCSPStapling` => scripted server 观察到 ClientHello 含 `status_request`
    - stapling enabled but not required + no stapled response => handshake success，`GetOCSPResponse=[]`，`IsOCSPResponseVerified=False`
    - stapling required + no stapled response => handshake fail-closed，错误文本包含 `OCSP` 或 `stapling`
    - stapling required + stapled `ocsp_response_successful_basic_v1.der` but still not accepted by bounded verifier => handshake fail-closed

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime`
- Expected:
  - FAIL because current FreePascal client neither sends `status_request` nor enforces required stapling policy

### Task 2: Implement Minimal Client Request / Surface / Required Policy

**Files:**
- Modify: `src/fafafa.ssl.tls13.wire.pas`
- Modify: `src/fafafa.ssl.tls13.clienthello.pas`
- Modify: `src/fafafa.ssl.tls13.servercertificate.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add bounded TLS constants and ClientHello plumbing**
- 在 `src/fafafa.ssl.tls13.wire.pas` 增加：
  - `TLS_EXTENSION_STATUS_REQUEST = $0005`
  - `TLS_CERT_STATUS_TYPE_OCSP = 1`
- 在 `src/fafafa.ssl.tls13.clienthello.pas`：
  - 增加 `status_request` extension builder
  - 让 `BuildTLS13ClientHelloBody(...)` 与 `BuildTLS13ClientHelloBodyWithPSKCore(...)` 根据布尔参数决定是否附带空 `status_request`
  - 保持现有 callsites 兼容，只在 FreePascal client path 按 context option 打开

**Step 2: Expose leaf CertificateEntry stapling data**
- 在 `src/fafafa.ssl.tls13.servercertificate.pas`：
  - 保留现有 `TryParseTLS13ServerCertificateHandshake(...)` 输出 DER array 的兼容面
  - 额外增加一个更详细但有界的 parser/result，至少暴露：
    - leaf DER
    - full DER array
    - leaf `status_request` extension 中的 stapled OCSP bytes
  - 结构校验至少覆盖：
    - handshake/body/list 长度一致
    - entry DER 长度合法
    - extension block 长度合法
    - `status_request` 若出现，则 body 必须是 `CertificateStatus` 结构：`status_type=ocsp` + `OCSPResponse<1..2^24-1>`

**Step 3: Cache OCSP state and enforce required policy**
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - 增加 connection-level OCSP state：
    - raw stapled response bytes
    - verification boolean
    - status string
  - 在 connect/start/close/failure path 上清空这些 state，避免旧连接泄漏
  - `ProbeServerHello` 组装 ClientHello 时，按 `ssoEnableOCSPStapling` 决定是否请求 stapling
  - `ProcessEncryptedServerFlight(...)` 解析 `Certificate` 后缓存 stapled response
  - 基于现有 peer leaf + issuer chain 调用 `TOCSPStaplingClient.ProcessStapledResponse(...)`
  - 新增 required-policy helper，只在 client full-handshake 且 `ssoRequireOCSPStapling` 时：
    - missing response => fail-closed
    - response present but not accepted by bounded verifier => fail-closed
  - 覆盖 `DoGetOCSPStaplingEnabled` / `DoGetOCSPResponse` / `DoIsOCSPResponseVerified` / `DoGetOCSPResponseStatus`

**Step 4: Preserve scope honesty**
- 不实现：
  - online AIA OCSP fetch
  - responder signature / issuer trust chain cryptographic verification
  - CT / SCT parsing
  - capability wording 更新
- 如果 stapled response 存在但当前有界 verifier 不能接受：
  - 在非 required 模式下只 surface 为 `verified = False`
  - 在 required 模式下 fail-closed

**Step 5: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Re-Run Adjacent Regressions And Gate

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Re-run focused and adjacent FreePascal regressions**
- Run:
  - `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime`
  - `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime`
  - `mkdir -p tmp/freepascal_client_cert_verify_flags_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_cert_verify_flags_runtime -FEtmp/freepascal_client_cert_verify_flags_runtime -otmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime tests/test_freepascal_client_cert_verify_flags_runtime.pas && ./tmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime`
  - `mkdir -p tmp/freepascal_client_peer_certificate_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_peer_certificate_surface -FEtmp/freepascal_client_peer_certificate_surface -otmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface tests/test_freepascal_client_peer_certificate_surface.pas && ./tmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface`
  - `mkdir -p tmp/freepascal_client_certificate_flight_requirements && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_certificate_flight_requirements -FEtmp/freepascal_client_certificate_flight_requirements -otmp/freepascal_client_certificate_flight_requirements/test_freepascal_client_certificate_flight_requirements tests/test_freepascal_client_certificate_flight_requirements.pas && ./tmp/freepascal_client_certificate_flight_requirements/test_freepascal_client_certificate_flight_requirements`
  - `mkdir -p tmp/freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption -FEtmp/freepascal_client_session_resumption -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 2: Diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-09-freepascal-client-ocsp-stapling-surface-required-policy.md src/fafafa.ssl.tls13.wire.pas src/fafafa.ssl.tls13.clienthello.pas src/fafafa.ssl.tls13.servercertificate.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_ocsp_stapling_runtime.pas task_plan.md findings.md progress.md`
- Expected:
  - exit `0`

### Definition Of Done

- `ssoEnableOCSPStapling` 会让 FreePascal TLS 1.3 client 真正发送 `status_request`
- successful full handshake 后，connection 能通过 `ISSLOCSPStapling` surface 暴露 stapled OCSP response 与 verification/status state
- `ssoRequireOCSPStapling` 对 missing/unaccepted stapled response fail-closed
- 新的 runtime contract、相邻 FreePascal regressions、`compile_all_modules.py`、diff hygiene 全绿
- working-memory 文件记录了本批刻意保持的边界：不宣称完整 revocation parity，不修改 capability wording
