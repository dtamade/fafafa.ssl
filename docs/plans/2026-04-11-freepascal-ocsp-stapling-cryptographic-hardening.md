# FreePascal OCSP Stapling Cryptographic Hardening Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 FreePascal client 只有在 stapled OCSP 的 responder signature / issuer-chain / delegated responder 验证通过时才把结果表述成 cryptographically verified，并在 `required` 模式下对这类失败 fail-closed。

**Architecture:** 这批在现有 stapling parse/freshness/status checks 之上再加一层 OpenSSL-backed cryptographic verification。先在 runtime harness 上补一个“good status 但 signature/delegated responder verify 失败”的 RED，再最小扩展 `TOCSPStaplingClient` 与 `src/fafafa.ssl.openssl.api.ocsp.pas`，让 raw stapled response 在标成 `ossVerified` 前必须先通过 cryptographic verify。可选模式继续允许连接成功，但 surface 必须诚实地反映 unverified；required 模式继续 fail-closed。

**Tech Stack:** FreePascal (ObjFPC), `TOCSPStaplingClient`, `TOCSPStaplingResult`, OpenSSL OCSP bindings, `VerifyOCSPResponse(...)`, `tests/test_freepascal_client_ocsp_stapling_runtime.pas`, file-based working memory.

---

## Summary

- 当前 stapling path 已经具备：
  - raw response surface
  - freshness checks
  - `good/revoked/unknown` acceptance semantics
  - optional / required boundary
- 但 `src/fafafa.ssl.ocsp.stapling.pas` 还缺一层关键语义：
  - `ProcessStapledResponse(...)` 只解析 DER、查 `CertID`、看 freshness / cert status
  - 还没有对 raw response 做 responder signature / chain / delegated responder verification
- 因此这批要收的是“verified 的真实性”，不是再扩新的 stapling source 或 online OCSP path。

## Delivery Order

1. 在 `tests/test_freepascal_client_ocsp_stapling_runtime.pas` 先补 cryptographic-failure RED。
2. 最小扩展 OpenSSL OCSP helper，让 stapling path 能复用 cryptographic verify。
3. 在 `TOCSPStaplingClient` 把 cryptographic verify 接到 `ossVerified` 之前。
4. 跑 focused GREEN、邻近 regressions、compile gate、diff hygiene。

### Task 1: RED - Reproduce cryptographic stapling drift

**Files:**
- Modify: `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- Reference: `src/fafafa.ssl.ocsp.stapling.pas`
- Reference: `src/fafafa.ssl.openssl.api.ocsp.pas`

**Step 1: Add deterministic cryptographic-failure fixtures or stubs**
- 在现有 runtime harness 上增加一条更细的 failure surface，优先选择下列任一最小方案：
  - stub `OCSP_BASICRESP_verify(...)` 让 good-status response 在 cryptographic 层失败；
  - 或构造 delegated responder 不满足 `OCSPSigning` / issuer-chain trust 的 fixture。
- 目标不是重写整套 OCSP fixture，而是稳定命中“status 看起来 good，但 crypto verify 应失败”的分支。

**Step 2: Add focused RED contracts**
- 新增 contract，例如：
  - `TestOptionalStaplingDoesNotSurfaceCryptoFailureAsVerified`
  - `TestRequiredStaplingFailsClosedWhenCryptoVerifyFails`
- 断言：
  - optional 模式下 handshake 可以继续，但 status 不得再说成 `Verified`
  - required 模式下 handshake 必须失败
  - verify result string 应包含 `OCSP` / `signature` / `responder` / `stapling`

**Step 3: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime`
- Expected:
  - FAIL，且失败点落在 current stapling path 仍把 parse-level success 当成 verified

### Task 2: Expose a bounded OCSP cryptographic verifier for stapling

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.ocsp.pas`

**Step 1: Reuse existing OCSP verification primitives**
- 复用已经存在的：
  - `OCSP_RESPONSE_get1_basic`
  - `OCSP_BASICRESP_verify`
  - issuer cert stack
  - verification store handling
- 如果当前 helper 只有 `Boolean` 返回值且诊断太弱，允许新增一个最小的 richer result / error string helper，但不要把整个 API 面一起重构。

**Step 2: Keep scope on stapled raw response verification**
- 只支持：
  - raw stapled OCSP DER response
  - leaf + issuer pair
  - bounded verify reason
- 不在这批顺手重写 online fetch helper。

### Task 3: Gate `ossVerified` behind cryptographic verification

**Files:**
- Modify: `src/fafafa.ssl.ocsp.stapling.pas`

**Step 1: Keep parse/freshness/status checks intact**
- 保留当前：
  - DER parse
  - matching `CertID`
  - freshness
  - non-good cert status reject

**Step 2: Insert cryptographic verification before success mapping**
- 在 `ProcessStapledResponse(...)` 里：
  - 只有 parse-level checks 通过后，才调用新的 OpenSSL OCSP verify helper
  - verify 失败时：
    - `Result.Status := ossVerificationFailed`
    - `Result.ErrorMessage` 明确指向 cryptographic failure
  - 只有 cryptographic verify 也通过后，才允许 `Result.Status := ossVerified`

**Step 3: Preserve optional / required semantics**
- optional:
  - 连接可以继续
  - 但 status / verify string 必须诚实反映 unverified
- required:
  - `ValidateStaplingRequirement(...)` 继续 fail-closed
- 不把这批扩成 CT source parity 或 online OCSP hardening。

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
  - `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime`
  - `mkdir -p tmp/freepascal_client_ct_sct_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ct_sct_surface -FEtmp/freepascal_client_ct_sct_surface -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface tests/test_freepascal_client_ct_sct_surface.pas && ./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface`
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
  - `git diff --check -- docs/plans/2026-04-11-freepascal-ocsp-stapling-cryptographic-hardening.md src/fafafa.ssl.ocsp.stapling.pas src/fafafa.ssl.openssl.api.ocsp.pas tests/test_freepascal_client_ocsp_stapling_runtime.pas task_plan.md findings.md progress.md`
- Expected:
  - PASS

### Definition Of Done

- stapled OCSP 的 `Verified` 状态不再只建立在 parse/freshness/status success 上。
- optional / required path 对 cryptographic failure 的表述和 gate 一致。
- 这批没有扩到 online OCSP，也没有顺手收 CT source parity。
