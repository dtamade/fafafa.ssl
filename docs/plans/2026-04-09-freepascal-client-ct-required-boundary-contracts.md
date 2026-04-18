**Goal:** 把 `CT required` 在两个关键边界上的契约补齐并跑实：`verify-none` 不应触发 CT request / fail-closed，`session resumption` 不应因为 resumed flight 缺少证书或 SCT material 而被 `CT required` 阻断。

**Why This Batch:** 上一批已经把 `CT required` 的负向 fail-closed 主路径打通，但还没有把两个显式 guard 写成回归契约：
- `sslVerifyPeer` 关闭时，`ValidateClientCertificateTransparency` 应直接跳过
- `FSessionReused=True` 时，resumed client path 应直接跳过

**Guardrails:**
- 不扩到 OCSP-delivered SCT source
- 不补正向可通过 CT policy 的 fixture
- 不改变 `ClientHello` 请求 SCT 的触发总原则：仍由 `sslVerifyPeer` 驱动
- 如果 focused tests 直接为绿，本批只收测试与 ledger，不强行改生产代码

---

## Task 1: RED - Add missing boundary contracts

**Files:**
- Modify: `tests/test_freepascal_client_ct_sct_surface.pas`
- Modify: `tests/test_freepascal_client_session_resumption.pas`

**Step 1: Add verify-none boundary contract**
- 在 `tests/test_freepascal_client_ct_sct_surface.pas` 新增场景：
  - context 显式 `SetVerifyMode([])`
  - 同时设置 `ssoRequireCertificateTransparency`
  - 服务端不提供 SCT
  - 断言：
    - `Connect = True`
    - `ObservedSCTRequest = False`
    - 不因为 `required` 触发 CT fail-closed

**Step 2: Add resumed-path boundary contract**
- 在 `tests/test_freepascal_client_session_resumption.pas` 新增场景：
  - 首次握手仍走现有 `verify-none` 离线 session capture
  - 第二次 resumed 握手改成：
    - `SetVerifyMode([sslVerifyPeer])`
    - 打开 `ssoRequireCertificateTransparency`
  - 服务端 resumed flight 继续不发送证书 / SCT（保持当前 harness）
  - 断言：
    - `Connect = True`
    - `IsSessionReused = True`
    - `ObservedPskClientHello = True`
    - 不因 `CT required` 阻断 resumed path

**Focused RED Commands:**
```bash
mkdir -p tmp/freepascal_client_ct_sct_surface && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_ct_sct_surface \
  -FEtmp/freepascal_client_ct_sct_surface \
  -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface \
  tests/test_freepascal_client_ct_sct_surface.pas && \
./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface
```

```bash
mkdir -p tmp/freepascal_client_session_resumption && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_session_resumption \
  -FEtmp/freepascal_client_session_resumption \
  -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption \
  tests/test_freepascal_client_session_resumption.pas && \
./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption
```

**Expected Outcome:**
- 若任一契约失败，说明 guard 仍有缺口或发生行为漂移，进入 Task 2 做最小修复。
- 若两个契约都直接为绿，说明实现已满足该边界；本批收口为 contract-only，不写生产代码。

---

## Task 2: GREEN - Only if the new contracts fail

**Files (only if RED observed):**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Possibly modify related helper wiring only if the failure point明确且可收窄

**Rules:**
- 只修复 focused RED 指向的最小 guard 缺口
- 不改 CT source precedence
- 不改 builder / option surface
- 不改 unrelated trust / OCSP / CertificateVerify / early-data logic

---

## Task 3: Verification / Closeout

**Commands:**
```bash
mkdir -p tmp/freepascal_client_ct_sct_surface && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_ct_sct_surface \
  -FEtmp/freepascal_client_ct_sct_surface \
  -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface \
  tests/test_freepascal_client_ct_sct_surface.pas && \
./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface
```

```bash
mkdir -p tmp/freepascal_client_session_resumption && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_session_resumption \
  -FEtmp/freepascal_client_session_resumption \
  -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption \
  tests/test_freepascal_client_session_resumption.pas && \
./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption
```

```bash
python3 scripts/compile_all_modules.py
```

```bash
git diff --check -- docs/plans/2026-04-09-freepascal-client-ct-required-boundary-contracts.md tests/test_freepascal_client_ct_sct_surface.pas tests/test_freepascal_client_session_resumption.pas task_plan.md findings.md progress.md
```

**Done When:**
- 两个边界契约被写实并有 fresh test evidence
- 若无 RED，则明确记录“contract-only closeout，无生产代码变更”
- 若有 RED，则最小修复后 focused tests 与 compile gate 为绿

---

## Execution Result

- 两个新契约都直接为绿，没有出现新的 runtime guard 缺口：
  - `verify-none + ssoRequireCertificateTransparency` => PASS
  - `resumed + verify-peer + ssoRequireCertificateTransparency` => PASS
- 因而本批按预设 guardrail 收口为 **contract-only**：
  - 只新增测试契约
  - 不改任何生产代码

## Final Verification

- `tests/test_freepascal_client_ct_sct_surface.pas` => PASS
  - 新增 `TestRequiredCertificateTransparencyIsIgnoredWhenVerifyPeerDisabled`
- `tests/test_freepascal_client_session_resumption.pas` => PASS
  - 新增 `TestResumedSessionSkipsRequiredCertificateTransparency`
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-client-ct-required-boundary-contracts.md tests/test_freepascal_client_ct_sct_surface.pas tests/test_freepascal_client_session_resumption.pas task_plan.md findings.md progress.md` => PASS

## Notes

- fresh evidence 说明当前实现已经稳定满足两个边界：
  - `sslVerifyPeer` 关闭时，`required` 不会触发 SCT request / fail-closed
  - resumed PSK 路径上，`required` 不会因为缺少 certificate / SCT server flight 而阻断握手
