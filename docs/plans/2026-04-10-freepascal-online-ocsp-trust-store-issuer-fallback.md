# FreePascal Online OCSP Trust-Store Issuer Fallback Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 FreePascal client 在 `sslVerifyPeer + sslCertVerifyCheckOCSP` 的 online OCSP 路径上，即使服务端只发送 leaf 证书、issuer 只存在于本地 trust store，也能解析出正确 issuer 并继续做 bounded fail-closed OCSP 校验。

**Architecture:** 这批继续保持很窄的边界，只收 `issuer material fallback`。先在现有 scripted TLS 1.3 online-OCSP runtime harness 上增加一个 “leaf-only server chain + CAFile trust store” 的 RED，证明当前实现会把 leaf 自己当 issuer 从而误失败；然后只在 `TFreePascalConnection` 里新增一个 issuer 解析 helper，优先使用 peer chain，其次回退到 context verification store。不中途扩到 CT source parity、server-side stapling，或更大的证书验证面。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `IFreePascalContextTrustStore`, `ISSLCertificateStore`, `tests/test_freepascal_client_online_ocsp_runtime.pas`, `fafafa.ssl.openssl.api.ocsp`, file-based working memory.

---

## Summary

- 当前 FreePascal client online OCSP 已经具备：
  - `sslCertVerifyCheckOCSP` full-handshake 接线
  - AIA responder URL 提取
  - context HTTP hooks transport 注入
  - `good` 放行、`revoked/unknown/error` fail-closed
- 但 issuer 来源还停在过窄假设：
  - `TryBuildPeerOCSPCertificatePair(...)` 只认 `FPeerCertificateChain[1]`
  - 如果 peer chain 里没有 issuer，就退回 `FPeerCertificate`
  - 这会让 “trust store 已能验证该 leaf，但在线 OCSP 仍把 leaf 当 issuer” 的路径误失败
- 这批最小正确动作因此是：
  - 用 focused runtime RED 固化 leaf-only server chain 场景
  - 新增一个优先 peer chain、其次 trust store 的 issuer fallback helper
  - 只接到 online OCSP path，不顺手扩 CT 或 capability wording

## Task 1: RED - Lock the leaf-only server-chain contract

**Files:**
- Modify: `tests/test_freepascal_client_online_ocsp_runtime.pas`
- Reference: `tests/test_freepascal_client_chain_trust_runtime.pas`

**Step 1: Add a leaf-only server material path**
- 在现有 CA-signed server material helper 上增加一个可选模式：
  - 保留仅 leaf PEM
  - 不把 CA cert 拼回 server certificate blob
- client 继续使用：
  - `.WithVerifyPeer`
  - `.WithCAFile('tests/certificate/test_certs/ca_cert.pem')`
  - `.WithHTTPHooks(...)`
  - `SetCertVerifyFlags([sslCertVerifyCheckOCSP])`

**Step 2: Add focused RED**
- 新增 runtime contract，例如：
  - `TestOnlineOCSPGoodStatusUsesTrustStoreIssuerFallbackWhenServerOmitsIssuer`
- 断言：
  - server 只发 leaf 时，client trust verification 仍能过
  - online OCSP `good` 状态也应继续成功
  - HTTP POST hook 仍被调用一次，证明 OCSP path 真实运行
- 当前预期：
  - FAIL，且 failure 不是 trust-store 本身缺失，而是 online OCSP issuer 解析过窄导致的误失败

**Step 3: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_client_online_ocsp_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_online_ocsp_runtime -FEtmp/freepascal_client_online_ocsp_runtime -otmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime tests/test_freepascal_client_online_ocsp_runtime.pas && ./tmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime`
- Expected:
  - FAIL，指向 leaf-only server chain 下 online OCSP issuer fallback 缺失

## Task 2: GREEN - Resolve issuer from trust store when peer chain is incomplete

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add a narrow issuer resolution helper**
- 新增 helper，例如：
  - `TryResolvePeerIssuerCertificate(out AIssuer: ISSLCertificate; out AError: string): Boolean`
- 逻辑顺序：
  - 若 `FPeerCertificateChain[1]` 存在，继续优先使用它
  - 否则若 context 支持 `IFreePascalContextTrustStore`：
    - 调 `BuildVerificationStore`
    - 用 `FindBySubject(FPeerCertificate.GetIssuer)` 查 issuer
  - 找不到再 fail，不能继续把 leaf 自己当 issuer

**Step 2: Wire helper into online OCSP path**
- `TryBuildPeerOCSPCertificatePair(...)` 改为走新的 issuer 解析 helper
- 保持行为边界：
  - 只改变 issuer material 选择
  - 不改 AIA 提取、HTTP hooks、OCSP status mapping

**Step 3: Run focused GREEN**
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
  - `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime`
  - `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime`
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
  - `git diff --check -- docs/plans/2026-04-10-freepascal-online-ocsp-trust-store-issuer-fallback.md src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_online_ocsp_runtime.pas task_plan.md findings.md progress.md`
- Expected:
  - PASS

## Notes

- 这批不把 FreePascal online OCSP 写成“完整 issuer-chain cryptographic parity 已收口”；这里只是让 issuer material 的来源不再局限于 peer chain。
- 这批不顺手接 `RefreshCertificateTransparencyValidationState(...)`；如果后续要让 CT validation 也吃同一套 issuer fallback，再单开一批。
