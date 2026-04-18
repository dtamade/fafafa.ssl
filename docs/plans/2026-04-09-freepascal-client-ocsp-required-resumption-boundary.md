**Goal:** 把 `ssoRequireOCSPStapling` 在 resumed TLS 1.3 client path 上的边界契约补齐并跑实，确保 resumed flight 缺少 certificate / stapled response 时不会被 required OCSP 误伤。

**Why This Batch:** 当前 FreePascal OCSP required 主路径已经落地，但和相邻 trust / flags / CT helper 相比，`ValidateClientOCSPStapling` 还没有显式 resumed guard：
- `ValidateClientPeerCertificateTrust` 在 `FSessionReused=True` 时直接跳过
- `ValidateClientPeerCertificateFlags` 在 `FSessionReused=True` 时直接跳过
- `ValidateClientCertificateTransparency` 在 `FSessionReused=True` 时直接跳过
- 但 `ValidateClientOCSPStapling` 仍会在 resumed path 上把“没有 stapled response”当成 required 失败

这和 TLS 1.3 resumed flight 的现实不匹配：resumed PSK path 本来就不会重新发送 certificate / stapled OCSP material。

**Guardrails:**
- 这批只处理 resumed boundary
- 不扩到 verify-none 语义
- 不修改 OCSP request trigger 规则
- 不扩到 online OCSP fetch / validation hardening

---

## Task 1: RED - Add the missing resumption boundary contract

**Files:**
- Modify: `tests/test_freepascal_client_session_resumption.pas`

**Step 1: Add resumed OCSP-required contract**
- 复用现有离线 session resumption harness：
  - 首次握手继续走 current session capture path
  - 第二次 resumed 握手改成：
    - `SetVerifyMode([sslVerifyPeer])`
    - 打开 `ssoRequireOCSPStapling`
  - 服务端 resumed flight 继续不发送 certificate / stapled response（保持当前 harness）
- 断言：
  - `Connect = True`
  - `IsSessionReused = True`
  - `ObservedPskClientHello = True`
  - `ObservedTicketIdentityMatch = True`
  - 不因 `required OCSP` 阻断 resumed path

**Focused RED Command:**
```bash
mkdir -p tmp/freepascal_client_session_resumption && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_session_resumption \
  -FEtmp/freepascal_client_session_resumption \
  -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption \
  tests/test_freepascal_client_session_resumption.pas && \
./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption
```

---

## Task 2: GREEN - Minimal runtime guard

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Skip required OCSP on resumed session**
- 在 `ValidateClientOCSPStapling` 里最小补上 resumed guard：
  - `FSessionReused=True` => 直接 `Exit(True)`
- 不改 verify-none 行为
- 不改 full-handshake required semantics
- 不改 OCSP state surface / verifier 逻辑

---

## Task 3: Verification / Closeout

**Commands:**
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
git diff --check -- docs/plans/2026-04-09-freepascal-client-ocsp-required-resumption-boundary.md src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_session_resumption.pas task_plan.md findings.md progress.md
```

---

## Execution Result

- RED 先由 `tests/test_freepascal_client_session_resumption.pas` 的
  `TestResumedSessionSkipsRequiredOCSPStapling` 复现：
  - resumed TLS 1.3 client path 在 `verify-peer + ssoRequireOCSPStapling`
    下，会因为没有新的 certificate / stapled-response flight 而被误判失败
- 最小生产修复只改了 `src/fafafa.ssl.freepascal.connection.pas`：
  - 在 `ValidateClientOCSPStapling` 增加 `FSessionReused` guard
  - `FSessionReused=True` 时直接 `Exit(True)`
- 没有扩展：
  - verify-none 语义
  - full-handshake required semantics
  - OCSP verifier / online fetch / state surface

## Final Verification

- `mkdir -p tmp/freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption -FEtmp/freepascal_client_session_resumption -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-client-ocsp-required-resumption-boundary.md src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_session_resumption.pas task_plan.md findings.md progress.md` => PASS
