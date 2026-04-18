# FreePascal Validation Next Wave Roadmap Closeout

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 记录 2026-04-11 这条 FreePascal validation next-wave 路线已经按顺序完成，并把后续补充 closeout 一并收口到完成态。

**Architecture:** 原路线图里的 5 个批次已经全部按 TDD 独立落地：先补 CT issuer-source evidence，再收 OCSP-delivered CT source parity，然后分别收 stapled / online OCSP cryptographic truth，最后收 remaining cert verify flags runtime parity。随后追加的 server-side OCSP stapling public closeout 也已经完成，所以当前重点只剩把 capability、focused gate 和文档 truth 统一到最终完成态。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `TOCSPResponse`, `TSCTValidator`, `TSSLCertificateChainVerifier`, OpenSSL OCSP/CT bindings, scripted TLS 1.3 runtime tests, file-based working memory.

---

## Closeout Summary

- 2026-04-11 规划的 5 个批次均已完成，不再处于 future queue：
  1. `docs/plans/2026-04-11-freepascal-ct-issuer-source-evidence-hardening.md`
  2. `docs/plans/2026-04-11-freepascal-ocsp-delivered-ct-source-parity.md`
  3. `docs/plans/2026-04-11-freepascal-ocsp-stapling-cryptographic-hardening.md`
  4. `docs/plans/2026-04-11-freepascal-online-ocsp-broader-hardening.md`
  5. `docs/plans/2026-04-11-freepascal-client-remaining-cert-verify-flags-parity.md`
- 这 5 批关闭后，旧 `KnownIssues` 里的 3 条宽泛表述都已过时：
  - `broader OCSP validation hardening`
  - `OCSP-delivered Certificate Transparency source parity`
  - `broader certificate validation hardening`
- 当前 capability truth 应该收敛为更窄的一条：
  - `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
- 随后追加的 `docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-public-closeout.md` 也已经完成：
  - server-side OCSP stapling issuance 不再是剩余 gap
  - public optional interface `ISSLServerOCSPStaplingContext` 已落地
  - builder file-based config `WithServerOCSPStapledResponseFile(...)` 已落地
- `scripts/run_freepascal_tls13_completeness_gate.sh` 现在应该覆盖 TLS 1.3 主线 + validation runtime focused lanes，而不再只是 TLS 1.3 smoke/capability surface。

## Completed Batch Ledger

### Batch 1: CT issuer-source evidence hardening

- 状态：已完成
- 结果：CT validation path 改为复用真实 issuer resolver，并有 stronger runtime evidence 证明不再把 leaf 自己误当 issuer。

### Batch 2: OCSP-delivered CT source parity

- 状态：已完成
- 结果：FreePascal client CT runtime surface 现在可以从 TLS extension、embedded X.509、OCSP-delivered SCT 三处取源。

### Batch 3: Stapled OCSP cryptographic hardening

- 状态：已完成
- 结果：stapled response 只有在 cryptographic verification 通过时才会 surface `verified = True`；required path 对 verification failure fail-closed。

### Batch 4: Online OCSP broader hardening

- 状态：已完成
- 结果：online OCSP 不再把 `good` status 与 `verified` 混为一谈；cryptographic / responder verification failure 会明确 fail-closed 并 surface 原因。

### Batch 5: Remaining cert verify flags runtime parity

- 状态：已完成
- 结果：`sslCertVerifyStrictChain` / `sslCertVerifyCheckRevocation` / `sslCertVerifyCheckCRL` 不再被静默忽略，并且 unavailable truth 会 fail-closed surface。

## What Changed After Closeout

- `KnownIssues` 不再继续引用已关闭的 Batch 2 / 4 / 5。
- focused gate 应纳入：
  - `tests/test_freepascal_revocation_fast_contracts.pas`
  - `tests/test_freepascal_client_chain_trust_runtime.pas`
  - `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
  - `tests/test_freepascal_client_online_ocsp_runtime.pas`
  - `tests/test_freepascal_client_ct_sct_surface.pas`
  - `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
- OCSP / CT 文档要改成完成态，而不是“未来将支持”的口径。

## Next Stage

- 本路线图已经收口，不再继续追加第 6 批。
- 原先单独立项的 server-side OCSP stapling issuance 已经通过 public closeout 计划完成，不再作为 future queue。
- 当前如果继续推进，应另起新计划处理真正未关闭的主题，而不是回到这条已关闭路线。

## Definition Of Done

- 5 个原批次都保持已完成状态。
- capability / focused gate / docs truth 不再保留 future tense。
- 后续 public closeout 结果已并入最终 truth，不再遗留“下一阶段待做”表述。
