# FreePascal Client OCSP Stapling Validation Hardening Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收紧 FreePascal client 对 stapled OCSP `non-good` 证书状态的验证语义，确保 optional surface 不再把 `unknown/revoked` 之类的响应说成 `Verified`，且 `required` 模式会对这类响应 fail-closed。

**Architecture:** 这批继续保持 OCSP validation hardening 的窄边界，不扩到 online AIA OCSP fetch、responder signature/issuer-chain cryptographic verification、也不引入新的 server-side stapling 能力。做法是先在现有 scripted TLS 1.3 OCSP runtime harness 上写 focused RED，构造一个结构合法但 `CertStatus <> ocspGood` 的 stapled response，证明当前实现会把它 surface 成“verified-ish”并在 `required` 模式下误放行；然后只在 `TOCSPStaplingClient` 里最小收紧状态映射与 requirement gate，让 `non-good` 状态统一落为 verification failure。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `TOCSPStaplingClient`, pure Pascal OCSP/TLS 1.3 helpers, scripted `TStream` runtime test harness, file-based working memory.

---

## Summary

- 当前 FreePascal client 在 OCSP stapling 主路径上已经具备：
  - request `status_request`
  - raw stapled response surface
  - missing/unaccepted response 的 required fail-closed
- 但 verifier 还有一条真实 hardening 缺口：
  - `TOCSPStaplingClient.ProcessStapledResponse(...)` 在 freshness / CertID 都通过后，会先把结果标成 `ossVerified`
  - `TOCSPStaplingResult.IsValid` 虽然仍要求 `CertStatus = ocspGood`
  - 但 `ValidateStaplingRequirement(...)` 只看 `FLastResult.Status = ossVerified`
- 这会导致两个不一致：
  - optional surface 可能出现 `verified = False`，但状态文本仍是 `Verified`
  - required 模式可能把 `unknown/revoked` 这类 `non-good` stapled response 误当成可接受
- 这批只修这一条一致性问题：
  - `CertStatus <> ocspGood` => 统一视为 verification failure
  - optional 模式继续连接成功，但 status text 必须明确是 failure/unknown/revoked
  - required 模式对这类响应 fail-closed

## Task 1: RED - Lock the non-good stapled-response contract

**Files:**
- Modify: `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- Reference: `tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der`

**Step 1: Add a focused non-good fixture helper**
- 在 runtime test 里新增一个最小 helper：
  - 基于现有 `ocsp_response_successful_basic_v1.der`
  - 只把单个 `good` cert-status tag 改成 `unknown`
  - 保持 response 结构合法，避免这批退化成 malformed parser 测试
- helper 要求：
  - 如果找不到目标字节模式，测试直接失败
  - 返回修改后的 raw bytes，供 optional/required 两个场景共用

**Step 2: Add two focused contracts**
- 新增 optional 场景，例如：
  - `TestOptionalStapledResponseWithUnknownCertStatusSurfacesFailure`
  - 断言：
    - `Connect = True`
    - `ObservedStatusRequest = True`
    - `GetOCSPResponse = mutated fixture`
    - `IsOCSPResponseVerified = False`
    - `GetOCSPResponseStatus` 不能是误导性的 `Verified`，应明确反映 failure/unknown
- 新增 required 场景，例如：
  - `TestRequiredStaplingFailsWhenStapledResponseCertStatusIsUnknown`
  - 断言：
    - `Connect = False`
    - failure string 提到 `ocsp` / `stapling` / `unknown`

**Step 3: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime`
- Expected:
  - FAIL because current implementation still treats `non-good` cert status as `ossVerified` for requirement/status mapping

## Task 2: GREEN - Tighten stapling acceptance to ocspGood only

**Files:**
- Modify: `src/fafafa.ssl.ocsp.stapling.pas`

**Step 1: Reject non-good cert status during stapled-response processing**
- 在 `ProcessStapledResponse(...)`：
  - 保留现有 DER 解析 / OCSP response status / CertID / freshness 流程
  - 但在 freshness 通过后，新增最小 guard：
    - `SingleResp.CertStatus <> ocspGood` => `Status := ossVerificationFailed`
    - `ErrorMessage` 明确写出 `OCSP certificate status: <Good|Revoked|Unknown>` 里的非 good 状态
  - 只有 `ocspGood` 才落到 `ossVerified`

**Step 2: Align required gate with the real validity bit**
- 在 `ValidateStaplingRequirement(...)`：
  - 最小改成依赖 `FLastResult.IsValid`
  - 不再只检查 `Status = ossVerified`
- 保持其余行为不变：
  - 不改 cache / refresh
  - 不改 online fetch
  - 不改 responder signature verification 范围

**Step 3: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

## Task 3: Adjacent Verification / Closeout

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Re-run adjacent regressions**
- Run:
  - `mkdir -p tmp/freepascal_client_peer_certificate_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_peer_certificate_surface -FEtmp/freepascal_client_peer_certificate_surface -otmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface tests/test_freepascal_client_peer_certificate_surface.pas && ./tmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface`
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
  - `git diff --check -- docs/plans/2026-04-10-freepascal-client-ocsp-stapling-validation-hardening.md src/fafafa.ssl.ocsp.stapling.pas tests/test_freepascal_client_ocsp_stapling_runtime.pas task_plan.md findings.md progress.md`
- Expected:
  - PASS

## Notes

- 这批刻意不新增二进制 fixture 文件；优先在测试内对现有 success fixture 做最小 deterministic mutation。
- 这批也不修改 `src/fafafa.ssl.freepascal.lib.pas` 的 capability/known-issues 文案，等后续更完整的 validation hardening 收口后再判断是否值得调整。
