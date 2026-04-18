# FreePascal CT Issuer-Source Evidence Hardening Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为 FreePascal client CT validation 建立能区分 `peer-chain issuer` 与 `trust-store issuer` 的更强 runtime evidence，只有在真实 RED 证明 issuer source 错误时才最小修改生产代码。

**Architecture:** 这批优先补证据而不是先改实现。现有 `tests/test_freepascal_client_ct_sct_surface.pas` 的 dummy SCT harness 已经证明 leaf-only server-chain surface 可用，但还不足以证明 `RefreshCertificateTransparencyValidationState(...)` 内部到底吃的是哪个 issuer。实现顺序是：先把 CT validation harness 收紧成“能观测 issuer source”的 focused RED；如果 RED 出现，再让 CT path 复用已有 `TryResolvePeerIssuerCertificate(...)`；如果 fresh evidence 继续为绿，则只保留更强 contract，不改生产代码。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `TryResolvePeerIssuerCertificate(...)`, `RefreshCertificateTransparencyValidationState(...)`, OpenSSL CT function stubs/bindings, `tests/test_freepascal_client_ct_sct_surface.pas`, file-based working memory.

---

## Summary

- 上一批已经证明：在 leaf-only server-chain + `LoadCAFile(...)` 场景下，当前 bounded CT surface 不会直接退化成 `Validation unavailable`。
- 但这个结果还不够强，因为它只能证明 user-visible result 可用，不能证明内部 issuer source 已正确：
  - 当前 `RefreshCertificateTransparencyValidationState(...)` 仍是 `chain[1]`，否则退回 leaf
  - online OCSP 已经改成复用 `TryResolvePeerIssuerCertificate(...)`
  - CT path 还没有拿到同等级别的证据
- 这批必须先把“能观测实际 issuer source”的 harness 补出来，否则后续任何 CT issuer/source 改动都可能只是代码对称化，而不是证据驱动修复。

## Delivery Order

1. 在 `tests/test_freepascal_client_ct_sct_surface.pas` 做 issuer-distinguishable harness，避免继续靠 dummy success 外推。
2. 先跑 focused RED，看当前 CT path 是否真的把 leaf 当 issuer，或根本已经正确走到 trust-store issuer。
3. 只有当 RED 稳定出现时，才最小修改 `src/fafafa.ssl.freepascal.connection.pas`。
4. 跑 focused GREEN、邻近 FreePascal regressions、compile gate、diff hygiene。
5. 回填 `task_plan.md` / `findings.md` / `progress.md`，并决定是否推进 Batch 2。

### Task 1: Build an issuer-distinguishable CT harness

**Files:**
- Modify: `tests/test_freepascal_client_ct_sct_surface.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`
- Reference: `src/fafafa.ssl.ct.sct.pas`

**Step 1: Add a focused issuer-observation seam in the test harness**
- 不再只断言 `HasCertificateTransparencyValidationResult=True`。
- 在测试里新增一个更强的 CT validation path，优先选择下列任一最小方案：
  - stub 最少量的 OpenSSL CT entry points，记录 CT validator 实际收到的 issuer subject / fingerprint；
  - 或构造一个只有在真实 CA issuer 下才可通过、在 leaf self-issuer 下必定失败的 precert-SCT fixture。
- 目标是让测试能区分：
  - `issuer == peer leaf`
  - `issuer == trust-store CA`

**Step 2: Reuse the existing leaf-only server-chain setup**
- 继续使用：
  - `GenerateCASignedServerMaterial(..., AIncludeCAChain := False)`
  - client `.WithVerifyPeer`
  - `LoadCAFile('tests/certificate/test_certs/ca_cert.pem')`
- 保持 scope 在 CT issuer source，不顺手改 TLS / OCSP / trust verification 其他语义。

**Step 3: Add focused RED contracts**
- 新增 contract，例如：
  - `TestCTValidationUsesTrustStoreIssuerWhenServerOmitsIssuerChain`
- 断言：
  - trust verification 仍能通过
  - CT validation 收到的 issuer 应是 CA subject，而不是 leaf subject
  - 如果 issuer source 仍错误，失败信息必须能明确指出 `leaf` / `issuer` / `trust store`

**Step 4: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_client_ct_sct_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ct_sct_surface -FEtmp/freepascal_client_ct_sct_surface -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface tests/test_freepascal_client_ct_sct_surface.pas && ./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface`
- Expected:
  - 如果当前 CT issuer source 仍错误，则 FAIL，且失败点明确落在 issuer source mismatch
  - 如果当前实现已经正确，则 PASS，并说明本批只保留更强 contract

### Task 2: Only if RED exists, reuse the shared issuer resolver

**Files:**
- Modify if RED is real: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Keep CT validation scope narrow**
- 不改：
  - SCT source precedence
  - CT policy
  - required CT fail-closed semantics
  - OCSP / trust verification 其他路径

**Step 2: Replace the CT-specific issuer fallback**
- 如果 Task 1 稳定 RED：
  - 在 `RefreshCertificateTransparencyValidationState(...)` 里停止使用：
    - `chain[1]` else `leaf`
  - 改为复用：
    - `TryResolvePeerIssuerCertificate(...)`
  - 只修改 issuer source 解析，不改 validation status formatting

**Step 3: Run focused GREEN**
- Re-run Task 1 command
- Expected:
  - PASS
  - 新 contract 能稳定证明 issuer source 已对齐 trust-store fallback

### Task 3: Adjacent verification and closeout

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run adjacent regressions**
- Run:
  - `mkdir -p tmp/freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption -FEtmp/freepascal_client_session_resumption -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime`
  - `mkdir -p tmp/freepascal_client_online_ocsp_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_online_ocsp_runtime -FEtmp/freepascal_client_online_ocsp_runtime -otmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime tests/test_freepascal_client_online_ocsp_runtime.pas && ./tmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime`
- Expected:
  - PASS

**Step 2: Run compile gate**
- Run:
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 3: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-11-freepascal-ct-issuer-source-evidence-hardening.md src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_ct_sct_surface.pas task_plan.md findings.md progress.md`
- Expected:
  - PASS

### Definition Of Done

- CT runtime harness 已能明确区分 trust-store issuer 与 leaf self-issuer。
- 如果 issuer source 真有问题，则 `RefreshCertificateTransparencyValidationState(...)` 已最小收口到 `TryResolvePeerIssuerCertificate(...)`。
- 如果 fresh evidence 证明当前实现已正确，则该结论被记录下来，且不做无证据生产修复。
- focused contract、邻近 regressions、compile gate 与 diff hygiene 均通过。
