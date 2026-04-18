# FreePascal Client Remaining Cert Verify Flags Parity Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把 FreePascal client 还在 runtime 路径上被静默忽略的 `sslCertVerifyStrictChain` / `sslCertVerifyCheckRevocation` / `sslCertVerifyCheckCRL` 收成有界 parity，并把失败原因写成可验证的 runtime truth。

**Architecture:** 这批不再使用泛泛的 “broader certificate validation hardening” 表述，而是只收 3 个已经存在但尚未真正接线的 verify flags。先加 focused RED，证明这些 flag 目前只停在 context storage；然后最小修改 `TFreePascalConnection.ValidateClientPeerCertificateTrust(...)` 的 options mapping、`TSSLCertificateChainVerifier` 的结果语义，以及 `TFreePascalCertificate.VerifyEx(...)` 的 detailed result surface，让这些 flag 不再被静默吞掉。若某个 flag 在当前 bounded architecture 下仍做不到完整语义，必须 fail-closed 或给出明确 unavailable，而不是继续无声通过。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `TSSLCertificateChainVerifier`, `StrictChainVerifyOptions`, `TFreePascalCertificate.VerifyEx(...)`, `tests/test_freepascal_client_cert_verify_flags_runtime.pas` or a new focused test unit, file-based working memory.

---

## Summary

- 目前 FreePascal client 已有的 runtime parity：
  - `sslCertVerifyIgnoreHostname`
  - `sslCertVerifyIgnoreExpiry`
  - `sslCertVerifyAllowSelfSigned`
- 仍被静默忽略的 flags：
  - `sslCertVerifyStrictChain`
  - `sslCertVerifyCheckRevocation`
  - `sslCertVerifyCheckCRL`
- 已有的基础设施并不为空：
  - `src/fafafa.ssl.certchain.pas` 里已有 `StrictChainVerifyOptions`
  - `CheckCertificateRevocation(...)` 已有骨架
  - `src/fafafa.ssl.freepascal.lib.pas` 里已有 `VerifyEx(...)`
- 但当前连接层只把 runtime trust path 接成：
  - `cvoCheckSignature`
  - `cvoCheckCAConstraints`
  - optional `cvoAllowSelfSigned`
- 因此这批的最小正确动作不是全面重写 trust stack，而是让这 3 个 flag 不再继续“有枚举、没 runtime truth”。

## Delivery Order

1. 加 focused RED，锁定这 3 个 flag 当前确实被静默忽略。
2. 最小修改连接层的 verify option mapping。
3. 在 `certchain` / `VerifyEx` 上补齐必要的 detailed result 与 unavailable truth。
4. 跑 focused GREEN、邻近 regressions、compile gate、diff hygiene。

### Task 1: RED - Prove the remaining verify flags are still dropped

**Files:**
- Modify or Add: `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
- Add if separation is cleaner: `tests/test_freepascal_client_remaining_cert_verify_flags_runtime.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`
- Reference: `src/fafafa.ssl.certchain.pas`
- Reference: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Add a strict-chain runtime contract**
- 新增 focused case，例如：
  - `sslCertVerifyStrictChain` + leaf 缺失 server-auth EKU / strict-chain-required property => `Connect = False`
  - 同一 fixture 在不带 strict-chain flag 时维持当前 bounded 行为
- 如果现有 test certificate generator 还不能构造 strict-chain failure fixture，允许最小补 `src/fafafa.ssl.cert.utils.pas`，但不要顺手做更大的证书工具重构。

**Step 2: Add revocation / CRL truth contracts**
- 新增 focused case，目标不是立刻做完整外部 CRL 系统，而是先锁定“不再静默忽略”：
  - `sslCertVerifyCheckRevocation` 打开时，如果当前路径能得到明确 revoked verdict，则必须 fail-closed
  - 如果当前 bounded implementation 只能得到 `unavailable/unknown`，也必须返回明确 verification failure，而不是继续静默通过
  - `sslCertVerifyCheckCRL` 同理
- 如果 full runtime harness 对 revocation 很难稳定构造，可增加一条更小的 contract：
  - fake / test-double `ISSLCertificate` 或 focused unit contract，验证 flags 至少真正进入 `VerifyEx(...)` / chain verifier，而不是停在 context storage

**Step 3: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_client_cert_verify_flags_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_cert_verify_flags_runtime -FEtmp/freepascal_client_cert_verify_flags_runtime -otmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime tests/test_freepascal_client_cert_verify_flags_runtime.pas && ./tmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime`
- Expected:
  - FAIL，且失败点落在 `strict-chain/revocation/CRL` flags 仍未形成 runtime truth

### Task 2: Map remaining flags into the client trust path

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Expand verify-option mapping**
- 在 `ValidateClientPeerCertificateTrust(...)`：
  - 保留当前 `sslVerifyPeer` / `not FSessionReused` gate
  - `sslCertVerifyAllowSelfSigned` 继续映射到 `cvoAllowSelfSigned`
  - `sslCertVerifyStrictChain` 映射到基于 `StrictChainVerifyOptions` 的更严格 option set
  - `sslCertVerifyCheckRevocation` / `sslCertVerifyCheckCRL` 映射到 `cvoCheckRevocation`

**Step 2: Keep hostname / expiry boundaries stable**
- 不把这批和已经收完的 hostname / expiry runtime parity 混在一起。
- hostname / expiry 继续走 `ValidateClientPeerCertificateFlags(...)`。

### Task 3: Make revocation / CRL outcomes explicit instead of silently passing

**Files:**
- Modify: `src/fafafa.ssl.certchain.pas`
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Tighten `TSSLCertificateChainVerifier` result semantics**
- 当请求了 `cvoCheckRevocation` 时：
  - 不允许只留下 warning 然后整体继续当作 valid
  - 至少要把 `revoked` 与 `unavailable/unknown` 区分开
- 如果当前 bounded architecture 没有真正的 CRL material，也要让结果明确表现成 `verification unavailable` 或 fail-closed，而不是静默吞掉 flag。

**Step 2: Improve `TFreePascalCertificate.VerifyEx(...)` detailed result**
- `VerifyEx(...)` 当前只做：
  - store verify
  - expiry check
- 本批需要至少补齐：
  - `RevocationStatus`
  - 对 strict-chain / revocation / CRL flags 的结果可见性
- 注意避免在 `certchain -> VerifyEx -> certchain` 间引入递归；实现必须保持有界。

**Step 3: Preserve scope**
- 不在这批顺手扩成完整 browser-grade PKI validation。
- 不顺手拉进 OCSP / CT。

**Step 4: Run focused GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 4: Adjacent verification and closeout

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run adjacent regressions**
- Run:
  - `mkdir -p tmp/freepascal_client_cert_verify_flags_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_cert_verify_flags_runtime -FEtmp/freepascal_client_cert_verify_flags_runtime -otmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime tests/test_freepascal_client_cert_verify_flags_runtime.pas && ./tmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime`
  - `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime`
  - `mkdir -p tmp/freepascal_client_online_ocsp_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_online_ocsp_runtime -FEtmp/freepascal_client_online_ocsp_runtime -otmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime tests/test_freepascal_client_online_ocsp_runtime.pas && ./tmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime`
  - `mkdir -p tmp/freepascal_client_ct_sct_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ct_sct_surface -FEtmp/freepascal_client_ct_sct_surface -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface tests/test_freepascal_client_ct_sct_surface.pas && ./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface`
- Expected:
  - PASS

**Step 2: Run compile gate**
- Run:
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 3: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-11-freepascal-client-remaining-cert-verify-flags-parity.md src/fafafa.ssl.freepascal.connection.pas src/fafafa.ssl.certchain.pas src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_client_cert_verify_flags_runtime.pas task_plan.md findings.md progress.md`
- Expected:
  - PASS

### Definition Of Done

- `sslCertVerifyStrictChain` / `sslCertVerifyCheckRevocation` / `sslCertVerifyCheckCRL` 不再被静默忽略。
- strict-chain 有 focused runtime contract。
- revocation / CRL 至少有明确的 runtime truth，不再“flag 开了但还是默默通过”。
- 这批仍然保持在 bounded parity，不冒充完整 certificate validation closure。
