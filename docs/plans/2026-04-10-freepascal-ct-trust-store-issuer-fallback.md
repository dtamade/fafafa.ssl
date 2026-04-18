# FreePascal CT Trust-Store Issuer Fallback Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 FreePascal client 在已有 `ISSLCertificateTransparencyValidation` 路径上，即使服务端只发送 leaf 证书、issuer 只存在于本地 trust store，也仍能为 TLS/embedded SCT material 构造正确 issuer 并产出 bounded CT validation result，而不是退回 leaf 自己导致 `Validation unavailable`。

**Architecture:** 这批继续保持很窄的边界，只收 `RefreshCertificateTransparencyValidationState(...)` 的 issuer material fallback。先在现有 scripted TLS 1.3 CT runtime harness 上增加一个 “leaf-only server chain + CAFile trust store” 的 RED，证明当前实现会在 CT validation 路径上把 leaf 当 issuer，从而丢失 validation result；然后只在 `TFreePascalConnection` 里让 CT path 复用已经存在的 `TryResolvePeerIssuerCertificate(...)`。不顺手改 CT policy、SCT source、required fail-closed 语义，也不扩到更大的证书验证面。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `ISSLCertificateTransparencyValidation`, `IFreePascalContextTrustStore`, `tests/test_freepascal_client_ct_sct_surface.pas`, OpenSSL CT/X509 bindings, file-based working memory.

---

## Summary

- 当前 FreePascal client CT validation 已经具备：
  - TLS SCT / embedded SCT raw surface
  - optional validation result / policy status surface
  - required CT fail-closed boundary
- 但 `RefreshCertificateTransparencyValidationState(...)` 的 issuer 选择仍停在旧假设：
  - `FPeerCertificateChain[1]` 存在时使用 issuer
  - 否则直接退回 `FPeerCertificate`
- 这与刚收口的 online OCSP issuer resolver 已经产生分叉：
  - trust verification 能从 trust store 找到 issuer
  - online OCSP 现在也能从 trust store fallback
  - 只有 CT validation 仍可能在 leaf-only server chain 上把 leaf 当 issuer
- 这批最小正确动作因此是：
  - 用 focused runtime RED 固化 leaf-only server chain 场景
  - 让 CT validation 复用 `TryResolvePeerIssuerCertificate(...)`
  - 不改 CT policy / status mapping 本身

## Task 1: RED - Lock the leaf-only CT validation contract

**Files:**
- Modify: `tests/test_freepascal_client_ct_sct_surface.pas`

**Step 1: Add a leaf-only server material path**
- 在现有 CA-signed server material helper 上增加一个可选模式：
  - 保留仅 leaf PEM
  - 不把 CA cert 拼回 server certificate blob
- client 继续使用：
  - `.WithVerifyPeer`
  - `.LoadCAFile('tests/certificate/test_certs/ca_cert.pem')`
  - 现有 CT scripted stream / dummy SCT list

**Step 2: Add focused RED**
- 新增 runtime contract，例如：
  - `TestTLSSCTListUsesTrustStoreIssuerFallbackWhenServerOmitsIssuer`
- 断言：
  - server 只发 leaf 时，client trust verification 仍能过
  - 现有 TLS SCT validation surface 仍应给出 validation result
  - validation status 不应落成 `Validation unavailable`
- 当前预期：
  - FAIL，且失败点落在 CT validation result 缺失或 issuer fallback 缺失

**Step 3: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_client_ct_sct_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ct_sct_surface -FEtmp/freepascal_client_ct_sct_surface -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface tests/test_freepascal_client_ct_sct_surface.pas && ./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface`
- Expected:
  - FAIL，指向 leaf-only server chain 下 CT validation issuer fallback 缺失

## Task 2: GREEN - Reuse the shared issuer resolver in CT validation

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Keep leaf material loading unchanged**
- 继续用现有 `TryCreateOpenSSLCertificateFromCertificate(...)` 把 peer leaf materialize 成 OpenSSL `PX509`

**Step 2: Replace CT issuer fallback**
- `RefreshCertificateTransparencyValidationState(...)` 不再自己做：
  - `chain[1]` or leaf
- 改成：
  - 调 `TryResolvePeerIssuerCertificate(...)`
  - 然后把返回的 issuer materialize 成 OpenSSL `PX509`
- 保持边界：
  - 只改变 issuer 解析来源
  - 不改 CT validation options / policy / status formatting

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
  - `mkdir -p tmp/freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption -FEtmp/freepascal_client_session_resumption -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime`
  - `mkdir -p tmp/freepascal_client_online_ocsp_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_online_ocsp_runtime -FEtmp/freepascal_client_online_ocsp_runtime -otmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime tests/test_freepascal_client_online_ocsp_runtime.pas && ./tmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime`
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
  - `git diff --check -- docs/plans/2026-04-10-freepascal-ct-trust-store-issuer-fallback.md src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_ct_sct_surface.pas task_plan.md findings.md progress.md`
- Expected:
  - PASS

## Notes

- 这批不把 FreePascal CT validation 写成“完整 issuer-chain cryptographic parity 已收口”；这里只是让 issuer material 的来源不再局限于 peer chain。
- 这批不顺手改 online OCSP，因为那条路径已经在上一批收口。

## Execution Notes

- Task 1 的 focused contract 没有出现预期 RED：
  - `tests/test_freepascal_client_ct_sct_surface.pas` 新增 leaf-only server-chain 场景后，fresh run 直接为绿
  - 这说明当前 bounded CT validation surface 在 “server 只发 leaf + 本地 CAFile” 路径下不会退化成 `Validation unavailable`
- 因为缺口没有被复现，这批没有修改 `src/fafafa.ssl.freepascal.connection.pas`
- 保留下来的有效产出是一个更窄、更诚实的 runtime contract：
  - `TestTLSSCTListValidationStaysAvailableWhenServerOmitsIssuerChain`
  - 它固定了 leaf-only server chain 下的 user-visible CT behavior
- 同时也得到一个更准确的 engineering 结论：
  - 现有 dummy SCT harness 足以验证 leaf-only surface availability
  - 但不足以证明 CT validation 内部到底走的是 peer-chain issuer 还是 trust-store issuer
  - 如果后续真要证明“内部 issuer source parity”，需要 real/stubbed precert-SCT evidence，而不是继续猜测

## Final Verification

- `tests/test_freepascal_client_ct_sct_surface.pas` => PASS
- `tests/test_freepascal_client_session_resumption.pas` => PASS
- `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
- `tests/test_freepascal_client_online_ocsp_runtime.pas` => PASS
- `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- task_plan.md findings.md progress.md` => PASS
- `git diff --no-index --check -- /dev/null docs/plans/2026-04-10-freepascal-ct-trust-store-issuer-fallback.md` => clean output（`/dev/null` 比较返回 `1` 属预期）
- `git diff --no-index --check -- /dev/null tests/test_freepascal_client_ct_sct_surface.pas` => clean output（`/dev/null` 比较返回 `1` 属预期）
