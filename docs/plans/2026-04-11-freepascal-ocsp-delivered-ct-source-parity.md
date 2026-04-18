# FreePascal OCSP-Delivered CT Source Parity Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 FreePascal client 能从 OCSP response / matching single response extensions 中提取、surface 并验证 SCT list，使 CT source parity 不再只停留在 TLS extension / embedded X.509。

**Architecture:** 这批只收“OCSP-delivered SCT source”本身，不把 scope 扩到 OCSP cryptographic hardening。先在 `tests/test_freepascal_client_ct_sct_surface.pas` 做一个 deterministic OCSP-delivered SCT RED，再最小扩展 `TOCSPResponse` 的扩展解析、补齐 `TSCTValidator.ValidateFromOCSP(...)`，并让 `TFreePascalConnection` 在现有 SCT source 为空时把 `FOCSPResponse` 作为新的 bounded source。stapled / online OCSP 的 responder signature、delegated responder、verify store 等问题留给后续 Batch 3 / Batch 4。

**Tech Stack:** FreePascal (ObjFPC), `TOCSPResponse`, `TOCSPSingleResponse`, `TSCTValidator`, `TFreePascalConnection`, OpenSSL OCSP/CT bindings, `tests/test_freepascal_client_ct_sct_surface.pas`, file-based working memory.

---

## Summary

- 当前 CT source 已覆盖：
  - TLS `signed_certificate_timestamp` extension
  - embedded X.509 SCT extension
- 但 OCSP-delivered source 仍然断着：
  - `src/fafafa.ssl.ocsp.pas`
    - `ParseResponseData(...)` 只显式提取了 nonce
    - `ParseSingleResponse(...)` 没有继续解析 `singleExtensions`
  - `src/fafafa.ssl.ct.sct.pas`
    - `TSCTValidator.ValidateFromOCSP(...)` 还是空实现
  - `src/fafafa.ssl.freepascal.connection.pas`
    - 当前不会把 `FOCSPResponse` 作为 SCT source
- 这批的最小正确动作是让 OCSP-delivered SCT list 进入已有 CT surface / validation pipeline，而不是顺手把 OCSP 本身写成 fully hardened。

## Delivery Order

1. 在 CT surface runtime harness 上补一个只靠 OCSP-delivered SCT list 的 focused RED。
2. 扩 `TOCSPResponse` 以解析 response-level / single-response-level SCT extensions。
3. 实现 `TSCTValidator.ValidateFromOCSP(...)`，让 OCSP-delivered source 复用已有 `ValidateSCTList(...)`。
4. 在连接层把 `FOCSPResponse` 纳入 bounded SCT source。
5. 跑 focused GREEN、邻近 OCSP/CT 回归、compile gate、diff hygiene。

### Task 1: RED - Reproduce missing OCSP-delivered SCT surface

**Files:**
- Modify: `tests/test_freepascal_client_ct_sct_surface.pas`
- Reference: `src/fafafa.ssl.ocsp.pas`
- Reference: `src/fafafa.ssl.ct.sct.pas`

**Step 1: Add a deterministic OCSP-delivered SCT fixture path**
- 在测试里新增一个最小 OCSP response builder 或 mutation helper：
  - response status = successful
  - leaf `CertID` 匹配当前 scripted certificate
  - `responseExtensions` 或 matching `singleExtensions` 中带 `signed_certificate_timestamp` OID
  - 不依赖 TLS SCT extension，不依赖 embedded SCT extension
- 目标是让当前 CT surface 唯一可能吃到的 SCT source 就是 OCSP。

**Step 2: Add focused RED contracts**
- 新增 contract，例如：
  - `TestCTSurfaceUsesOCSPDeliveredSCTListWhenNoTLSSCTOrEmbeddedSCTExists`
  - `TestMalformedOCSPDeliveredSCTListDoesNotSurfaceAsValidated`
- 断言：
  - `GetSignedCertificateTimestampCount > 0`
  - `GetCertificateTransparencyStatus` 应明确说明 `OCSP`
  - validation result / policy status 能产生 bounded output
  - malformed OCSP-delivered SCT list 不能伪装成正常 validated status

**Step 3: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_client_ct_sct_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ct_sct_surface -FEtmp/freepascal_client_ct_sct_surface -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface tests/test_freepascal_client_ct_sct_surface.pas && ./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface`
- Expected:
  - FAIL，且失败点落在 OCSP-delivered SCT list 还没有进入 current CT source pipeline

### Task 2: Parse SCT-bearing OCSP extensions

**Files:**
- Modify: `src/fafafa.ssl.ocsp.pas`

**Step 1: Extend OCSP response models**
- 给 `TOCSPSingleResponse` 与/或 `TOCSPResponse` 增加与 SCT source 相关的最小字段，例如：
  - raw SCT list bytes
  - SCT count
  - `HasSignedCertificateTimestampList`
- 保持边界：
  - 不把整个 OCSP extension universe 一次性全补完
  - 只收 `signed_certificate_timestamp` 相关扩展

**Step 2: Parse response-level SCT extension**
- 在 `ParseResponseData(...)`：
  - 保留 nonce 解析
  - 额外识别 SCT OID
  - 提取 raw SCT list，并在可能时计算数量

**Step 3: Parse matching single-response SCT extension**
- 在 `ParseSingleResponse(...)`：
  - 继续下探 `singleExtensions [1] EXPLICIT Extensions`
  - 同样识别 SCT OID，并记录到单条 `TOCSPSingleResponse`
- 保持“只解析，不做 cryptographic acceptance”的范围。

### Task 3: Implement OCSP-backed CT validation

**Files:**
- Modify: `src/fafafa.ssl.ct.sct.pas`

**Step 1: Implement `ValidateFromOCSP(...)`**
- 让 `TSCTValidator.ValidateFromOCSP(...)` 不再直接返回空数组。
- 最小策略：
  - 从 OCSP response 中提取 SCT list
  - 解码成 `PSCT_LIST`
  - 复用 `ValidateSCTList(SCTs, Cert, Issuer)`

**Step 2: Define source precedence inside the OCSP parser/validator**
- 如果 response-level 和 matching single-response 同时存在：
  - 优先 matching single-response SCT list
  - response-level 作为 fallback
- 保持 scope：
  - 不在这批决定 stapled vs online 的更大 source precedence
  - 只解决“OCSP bytes 里携带了 SCT list，但当前完全吃不到”的缺口

### Task 4: Wire OCSP-delivered SCTs into the FreePascal client surface

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Keep existing source order as conservative as possible**
- 继续优先现有 source：
  - TLS SCT extension
  - embedded X.509 SCT extension
- 仅当以上 source 都为空时，再尝试从 `FOCSPResponse` 提取 OCSP-delivered SCT list。

**Step 2: Surface OCSP-delivered SCT list**
- 在连接层新增一个最小 helper，例如：
  - 从 `FOCSPResponse` 提取 SCT list
  - 填充 `FSignedCertificateTimestampList`
  - 填充 `FSignedCertificateTimestampCount`
  - 把 `FCertificateTransparencyStatus` 写成 `Received from OCSP response (...)`
- 保持 `RefreshCertificateTransparencyValidationState(...)` 作为统一 validation entry。

**Step 3: Run focused GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 5: Adjacent verification and closeout

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run adjacent OCSP / CT regressions**
- Run:
  - `mkdir -p tmp/freepascal_client_ct_sct_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ct_sct_surface -FEtmp/freepascal_client_ct_sct_surface -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface tests/test_freepascal_client_ct_sct_surface.pas && ./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface`
  - `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime`
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
  - `git diff --check -- docs/plans/2026-04-11-freepascal-ocsp-delivered-ct-source-parity.md src/fafafa.ssl.ocsp.pas src/fafafa.ssl.ct.sct.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_ct_sct_surface.pas task_plan.md findings.md progress.md`
- Expected:
  - PASS

### Definition Of Done

- OCSP-delivered SCT list 能进入 FreePascal client CT surface / validation pipeline。
- `TSCTValidator.ValidateFromOCSP(...)` 不再是空实现。
- `TOCSPResponse` 能最小解析 SCT-bearing OCSP extensions。
- 本批没有把 stapled / online OCSP cryptographic hardening 混进来。
