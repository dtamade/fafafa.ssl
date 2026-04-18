# FreePascal Online OCSP Broader Hardening Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把 FreePascal client online OCSP path 从“能发请求并区分 good/revoked/unknown”收紧到更可信的 responder verification、delegated responder 处理与 fail-closed truth。

**Architecture:** 这批不重做 online fetch parity，也不碰 CT source。先在 runtime harness 上增加 cryptographic verify / delegated responder failure 的 RED，再把 `src/fafafa.ssl.openssl.api.ocsp.pas` 从“只回一个 OCSP cert status 整数”收紧成“能返回验证结果与失败原因”的 bounded helper，最后在 `TFreePascalConnection.ValidateClientOnlineOCSP(...)` 上把 richer result 映射成明确的 fail-closed 错误。HTTP hooks、issuer fallback、AIA URL 提取沿用当前实现。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `CheckCertificateStatus(...)` and related OCSP helpers, OpenSSL OCSP bindings, `tests/test_freepascal_client_online_ocsp_runtime.pas`, file-based working memory.

---

## Summary

- 当前 online OCSP path 已经具备：
  - `sslCertVerifyCheckOCSP` runtime 接线
  - context HTTP hooks transport
  - leaf-only server-chain issuer fallback
  - `good/revoked/unknown` 的 fail-closed 基本语义
- 但还剩两类更细的 hardening gap：
  - 当前连接层只拿到一个 `Integer status`，看不到 richer verification reason
  - responder signature / delegated responder / trust-store verification failure 还没有被清晰映射到 runtime truth
- 这批的目标不是再扩 source，而是让 “online OCSP failed” 的原因与 fail-closed 行为都更真实。

## Delivery Order

1. 先在 runtime harness 上补 cryptographic / delegated-responder failure 的 RED。
2. 最小扩 `src/fafafa.ssl.openssl.api.ocsp.pas`，让 helper 返回 richer verification result。
3. 在 `ValidateClientOnlineOCSP(...)` 上映射 clearer fail-closed errors。
4. 跑 focused GREEN、邻近 regressions、compile gate、diff hygiene。

### Task 1: RED - Lock online OCSP cryptographic failure truth

**Files:**
- Modify: `tests/test_freepascal_client_online_ocsp_runtime.pas`
- Reference: `src/fafafa.ssl.openssl.api.ocsp.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add deterministic online-OCSP failure hooks**
- 在当前 deterministic HTTP POST + OpenSSL stub harness 上新增下列最小 failure surface：
  - `OCSP_BASICRESP_verify(...)` 失败
  - delegated responder 不满足 verify 条件
  - cryptographic verify 失败但 `OCSP_resp_find_status(...)` 仍返回 `good`
- 保持 transport / AIA URL / issuer fallback 仍为绿，避免把 RED 混成 fetch path 问题。

**Step 2: Add focused RED contracts**
- 新增 contract，例如：
  - `TestOnlineOCSPGoodStatusFailsClosedWhenCryptographicVerifyFails`
  - `TestOnlineOCSPDelegatedResponderFailureIsSurfacedClearly`
- 断言：
  - handshake 必须 fail-closed
  - verify result string 不应再只有泛泛的 `verification failed`
  - 错误文本应包含 `OCSP` / `signature` / `responder` / `delegated`

**Step 3: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_client_online_ocsp_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_online_ocsp_runtime -FEtmp/freepascal_client_online_ocsp_runtime -otmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime tests/test_freepascal_client_online_ocsp_runtime.pas && ./tmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime`
- Expected:
  - FAIL，且失败点落在 current online OCSP path 对 cryptographic failure 的 truth 仍不够细

### Task 2: Return richer OCSP verification results

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.ocsp.pas`

**Step 1: Keep the existing transport/fetch helper behavior**
- 继续复用：
  - `CreateOCSPRequest(...)`
  - `SendOCSPRequest(...)`
  - nonce / validity checks
  - issuer cert stack

**Step 2: Add a bounded detailed result helper**
- 新增一个最小 helper，例如：
  - `CheckCertificateStatusDetailed(...)`
  - 或 `TOCSPCheckResult`
- 至少返回：
  - cert status
  - `Verified: Boolean`
  - failure reason / stage
- 不在这批顺手开放一整套 public OCSP API 重构。

**Step 3: Keep delegated-responder / signature failures distinguishable**
- 对以下类型的失败给出不同 reason：
  - response status 非 successful
  - basic response unavailable
  - cryptographic verify 失败
  - status not found
  - validity / nonce failure

### Task 3: Map richer results into FreePascal runtime truth

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Replace bare integer-only status handling**
- `ValidateClientOnlineOCSP(...)` 不再只依赖 `Integer status`。
- 改为消费新的 richer helper result，并把：
  - `good + verified`
  - `revoked`
  - `unknown`
  - `verify unavailable / cryptographic failure`
  映射成更清楚的 handshake outcome。

**Step 2: Keep existing boundaries**
- 继续保持：
  - `sslVerifyPeer`
  - `not FSessionReused`
  - `sslCertVerifyCheckOCSP`
  - context HTTP hooks
  - current issuer fallback
- 不顺手扩 CT source 或 stapling path。

**Step 3: Run focused GREEN**
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
  - `mkdir -p tmp/freepascal_client_online_ocsp_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_online_ocsp_runtime -FEtmp/freepascal_client_online_ocsp_runtime -otmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime tests/test_freepascal_client_online_ocsp_runtime.pas && ./tmp/freepascal_client_online_ocsp_runtime/test_freepascal_client_online_ocsp_runtime`
  - `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime`
  - `mkdir -p tmp/freepascal_client_chain_trust_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_chain_trust_runtime -FEtmp/freepascal_client_chain_trust_runtime -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas && ./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime`
- Expected:
  - PASS

**Step 2: Run compile gate**
- Run:
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 3: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-11-freepascal-online-ocsp-broader-hardening.md src/fafafa.ssl.freepascal.connection.pas src/fafafa.ssl.openssl.api.ocsp.pas tests/test_freepascal_client_online_ocsp_runtime.pas task_plan.md findings.md progress.md`
- Expected:
  - PASS

### Definition Of Done

- online OCSP cryptographic / delegated responder failures 不再被压平。
- runtime fail-closed truth 能区分 `revoked/unknown` 与 `verify failed/unavailable`。
- 这批没有重做 fetch parity，也没有混入 CT source parity。
