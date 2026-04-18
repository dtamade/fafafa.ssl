# FreePascal Client Online OCSP Fetch Parity Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 FreePascal client 在 `sslVerifyPeer + sslCertVerifyCheckOCSP` 的 full-handshake 路径上，真正执行基于证书 AIA 的 online OCSP fetch，并通过 context HTTP hooks 走上层注入的传输；当 responder 报 `revoked/unknown`、AIA 缺失、transport 失败或验证不可用时，按当前有界语义 fail-closed。

**Architecture:** 这批继续保持 revocation parity 的窄边界，不改 OCSP stapling surface/required 语义，不扩到 responder signature / issuer-chain cryptographic parity、OCSP-delivered CT source parity、也不改 server-side 行为。实现分三层：
1. `TFreePascalContext` 补 `ISSLHttpHooksAccess`，让 builder / direct context 都能注入 HTTP hooks；
2. `TFreePascalConnection` 新增一个只在 client `verify-peer`、非 resumed full handshake 上运行的 online OCSP helper；
3. helper 复用现有 `GetOCSPURLFromCertificate(...)`、`TryCreateOpenSSLCertificateFromCertificate(...)`、`CheckCertificateStatus(...)`，并在需要时把 context hooks 临时推入 `fafafa.ssl.net.hooks` 的线程局部 scope。
测试使用 scripted TLS 1.3 server + deterministic HTTP POST hook + OCSP/OpenSSL function stubs，避免真实网络和 responder 依赖。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `TFreePascalContext`, `ISSLHttpHooksAccess`, `fafafa.ssl.openssl.api.ocsp`, `fafafa.ssl.net.hooks`, scripted `TStream` runtime harness, file-based working memory.

---

## Summary

- 当前 FreePascal client 已经具备：
  - peer certificate / chain runtime surface
  - trust / hostname / expiry runtime checks
  - stapled OCSP request / surface / required / bounded validation
- 但 `sslCertVerifyCheckOCSP` 这条在线 revocation path 还停在“能力周边已就位、连接层未接线”的状态：
  - `sslCertVerifyCheckOCSP` flag 已存在且 OpenSSL 后端已使用
  - `GetOCSPURLFromCertificate(...)` 已能从 AIA 提取 responder URL
  - `CreateOCSPRequest / SendOCSPRequest / VerifyOCSPResponse / CheckCertificateStatus` 已存在
  - `fafafa.ssl.net.hooks` 已提供 thread-local HTTP GET/POST hooks
  - 但 FreePascal client full-handshake verify 流程里还没有 online OCSP fetch
  - 同时 `TFreePascalContext` 目前也还没暴露 `ISSLHttpHooksAccess`
- 这批最小正确动作因此是：
  - FreePascal context 支持 HTTP hooks access
  - client verify path 在 `sslCertVerifyCheckOCSP` 打开时执行 AIA online OCSP fetch
  - 只保证 bounded/fail-closed parity，不顺手扩 broader validation surface

## Task 1: RED - Lock the online OCSP fetch contract

**Files:**
- Add: `tests/test_freepascal_client_online_ocsp_runtime.pas`
- Reference: `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- Reference: `tests/test_openssl_connection_posthandshake_ocsp_storectx_issuer_contract.pas`

**Step 1: Add a scripted FreePascal client runtime harness**
- 复用现有 scripted TLS 1.3 full-handshake 模式：
  - CA-signed leaf
  - leaf 带 `OCSPResponderURL`
  - client 真实走 `TFreePascalConnection.Connect`
- 通过 builder 建 context：
  - `.WithBackend(sslFreePascal)`
  - `.WithTLS13`
  - `.WithVerifyPeer`
  - `.WithHTTPHooks(...)`
  - build 后再 `SetCertVerifyFlags([sslCertVerifyCheckOCSP])`

**Step 2: Add deterministic HTTP/OCSP stubs**
- 新增 HTTP POST hook stub，记录：
  - 调用次数
  - URL
  - Content-Type
  - body 长度
- 新增最小 OpenSSL OCSP stub 组合，让 `CheckCertificateStatus(...)` 可 deterministic 地返回：
  - `good` => 连接成功
  - `revoked` => fail-closed
- 保持测试不访问真实网络、不依赖真实 responder。

**Step 3: Add focused contracts**
- `TestOnlineOCSPGoodStatusUsesContextHooksAndConnects`
  - 断言：
    - context 支持 `ISSLHttpHooksAccess`
    - `Connect = True`
    - HTTP POST hook 被调用
    - URL / Content-Type / body 被正确转发
- `TestOnlineOCSPRevokedStatusFailsClosed`
  - 断言：
    - `Connect = False`
    - failure string 提到 `ocsp` / `revoked`
    - HTTP POST hook 被调用，证明不是其他验证分支失败

**Step 4: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_client_online_ocsp_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_online_ocsp_runtime -FEtmp/freepascal_client_online_ocsp_runtime -otmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime tests/test_freepascal_client_online_ocsp_runtime.pas && ./tmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime`
- Expected:
  - FAIL，因为当前 FreePascal context 还不支持 `ISSLHttpHooksAccess`，且 client verify path 还没有执行 online OCSP fetch

## Task 2: GREEN - Wire FreePascal client online OCSP fetch

**Files:**
- Modify: `src/fafafa.ssl.freepascal.context.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Expose HTTP hooks on FreePascal context**
- 在 `TFreePascalContext`：
  - 实现 `ISSLHttpHooksAccess`
  - 增加 `FHTTPGetCallback` / `FHTTPPostCallback`
  - 提供 `Set/GetHTTPGetCallback` 与 `Set/GetHTTPPostCallback`
- 保持行为窄边界：
  - 只暴露 hooks storage，不改其他 context 配置语义

**Step 2: Add the smallest client-side online OCSP helper**
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - 新增 helper，例如 `ValidateClientOnlineOCSP`
  - guard：
    - `sslVerifyPeer in VerifyMode`
    - `not FSessionReused`
    - `sslCertVerifyCheckOCSP in CertVerifyFlags`
  - helper 逻辑：
    - 解析 peer leaf / issuer
    - 提取 AIA OCSP URL
    - 确保 OpenSSL OCSP module 可用
    - materialize OpenSSL `PX509` leaf / issuer
    - 如 context 暴露 HTTP hooks，则 push 到 `TSSLHTTPHooksScope`
    - 调用 `CheckCertificateStatus(...)`
    - `good` => success
    - `revoked` / `unknown` / error => fail-closed，并给出明确错误消息
- 不修改：
  - stapling surface
  - stapling required policy
  - CT validation
  - server-side behavior

**Step 3: Wire helper into client full-handshake verify flow**
- 在 `DoConnect` 的 verify sequence 中：
  - 保持 trust / flags / stapling / CT 的原有相对语义
  - 新 helper 放在 stapling 之后、CT 之前，确保 required-stapling 仍先收口

**Step 4: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

## Task 3: Adjacent Verification / Closeout

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run adjacent regressions**
- Run:
  - `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime`
  - `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime`
  - `mkdir -p tmp/freepascal_client_certificateverify_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_certificateverify_runtime -FEtmp/freepascal_client_certificateverify_runtime -otmp/freepascal_client_certificateverify_runtime/test_freepascal_client_certificateverify_runtime tests/test_freepascal_client_certificateverify_runtime.pas && ./tmp/freepascal_client_certificateverify_runtime/test_freepascal_client_certificateverify_runtime`
- Expected:
  - PASS

**Step 2: Run compile gate**
- Run:
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 3: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-10-freepascal-client-online-ocsp-fetch-parity.md src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_online_ocsp_runtime.pas task_plan.md findings.md progress.md`
- Expected:
  - PASS

## Notes

- 这批在线 OCSP 仍然依赖现有 OpenSSL OCSP helper；不把这次工作误表述成“纯 Pascal 完整 online revocation stack”。
- `CheckCertificateStatus(...)` 当前已经实现 fail-closed 的请求发送 / 响应验证 / 状态判定逻辑；本批重点是 FreePascal client 接线与 hook 注入，不重写 OCSP cryptographic verifier。
- capability `KnownIssues` 暂不更新，等 online OCSP fetch parity 实际收口并验证完成后再决定是否需要同步收紧文案。
