# FreePascal Client Cert Verify Flags Hostname Expiry Parity Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 pure Pascal TLS 1.3 client 在 `sslVerifyPeer` 下，对 full handshake 的对端证书真正执行 `sslCertVerifyIgnoreHostname` / `sslCertVerifyIgnoreExpiry` runtime parity，而不再只把 flags 停在 context storage。

**Architecture:** 这批继续保持 validation hardening 的窄边界：只在 `TFreePascalConnection.DoConnect` 的 non-resumed full-handshake 路径补 hostname/expiry runtime checks，复用上一批已经落下的 peer-certificate cache/surface 与 `ISSLCertificate.VerifyHostname` / `IsExpired`。这批不扩到链信任验证、OCSP/CT、client-side `CertificateVerify` signature verification，也不在 resumed PSK path 上伪造 peer-certificate persistence。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `ISSLCertificate`, `TSSLCertVerifyFlags`, TLS 1.3 scripted `TStream` tests, `TCertificateUtils` test-only certificate generation, file-based working memory.

---

## Summary

- 当前 FreePascal backend 已经能：
  - 在 full handshake 上要求看到 `Certificate` / `CertificateVerify`
  - 在连接对象上暴露 peer leaf certificate 与 chain
- 但 runtime verify flags 仍停在 storage 层：
  - `TFreePascalContext.SetCertVerifyFlags(...)` / `GetCertVerifyFlags(...)` 可用
  - `TFreePascalConnection.DoConnect` 没有调用 `VerifyHostname(...)`
  - 也没有调用 `IsExpired`
- 因此这批只收两条最小 parity：
  - hostname mismatch 在 `sslVerifyPeer` 且未设置 `sslCertVerifyIgnoreHostname` 时 fail-closed
  - expired certificate 在 `sslVerifyPeer` 且未设置 `sslCertVerifyIgnoreExpiry` 时 fail-closed
- 明确保留的 out-of-scope：
  - 链信任验证 / `ISSLCertificateStore` plumbing
  - OCSP stapling / Certificate Transparency
  - resumed PSK session persistence 对 peer certificate 的继承

## Delivery Order

1. 写 plan 与 working-memory 入口，锁定范围为 FreePascal client hostname/expiry runtime parity。
2. 先加 focused RED，证明当前 mismatch/expired full handshake 在 FreePascal client 下仍错误通过。
3. 最小实现只改 `src/fafafa.ssl.freepascal.connection.pas`。
4. 跑 focused tests、相邻 FreePascal regressions、`python3 scripts/compile_all_modules.py`、diff hygiene。
5. 回填 findings / progress / task plan，并把 queue 继续收回到链验证 / OCSP / CT。

### Task 1: Add RED Coverage For Hostname And Expiry Runtime Parity

**Files:**
- Add: `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`
- Reference: `tests/test_freepascal_client_peer_certificate_surface.pas`
- Reference: `tests/test_freepascal_client_session_resumption.pas`

**Step 1: Write the failing scripted full-handshake contracts**
- 新建 `tests/test_freepascal_client_cert_verify_flags_runtime.pas`：
  - 复用 scripted TLS 1.3 client-handshake stream 模式，驱动真实 `TFreePascalConnection.Connect`
  - 用 test-only helper 生成两组 leaf cert/key：
    - hostname mismatch leaf：SAN=`alt.example.com`，连接 `ServerName='example.com'`
    - expired leaf：`NotAfter < Now`
  - 两组证书都由已有 `tests/certificate/test_certs/ca_cert.pem` / `ca_key.pem` 签发，并把 issuer 拼进 `Certificate` handshake
  - 覆盖四个 focused cases：
    - default verify flags + hostname mismatch => `Connect = False`
    - `[sslCertVerifyIgnoreHostname]` + hostname mismatch => `Connect = True`
    - default verify flags + expired leaf => `Connect = False`
    - `[sslCertVerifyIgnoreExpiry]` + expired leaf => `Connect = True`
  - 对失败 case 额外断言：
    - `GetVerifyResult = Ord(sslErrHostnameMismatch)` 或 `Ord(sslErrCertificateExpired)`
    - `GetVerifyResultString` 包含 `hostname` 或 `expired`
- 保持 client context `SetVerifyMode([sslVerifyPeer])`，让测试直接锁在 runtime parity，不掺入 chain trust 语义。

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_client_cert_verify_flags_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_cert_verify_flags_runtime -FEtmp/freepascal_client_cert_verify_flags_runtime -otmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime tests/test_freepascal_client_cert_verify_flags_runtime.pas && ./tmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime`
- Expected:
  - FAIL because current FreePascal client full handshake still accepts hostname-mismatched / expired certificates even when `sslVerifyPeer` is enabled

### Task 2: Implement Minimal Hostname And Expiry Runtime Checks

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify if fixture truth requires it: `src/fafafa.ssl.cert.utils.pas`

**Step 1: Add a bounded client-side verify helper**
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - 增加一个只供 client full-handshake 路径使用的 helper
  - helper 读取：
    - `FContext.GetVerifyMode`
    - `FContext.GetCertVerifyFlags`
    - `FPeerCertificate`
    - `FServerName`
  - 行为：
    - 仅在 `sslVerifyPeer in VerifyMode` 且 `not FSessionReused` 时执行
    - 若没有 cached peer certificate，fail-closed 为 `sslErrCertificate`
    - 若未设置 `sslCertVerifyIgnoreHostname`：
      - `FServerName` 为空 => `sslErrHostnameMismatch`
      - `VerifyHostname(...) = False` => `sslErrHostnameMismatch`
    - 若未设置 `sslCertVerifyIgnoreExpiry`：
      - `IsExpired = True` => `sslErrCertificateExpired`

**Step 2: Wire helper into the successful client handshake path**
- 在 `DoConnect`：
  - 保持现有 `ProcessEncryptedServerFlight(...)` / `SendClientFinished(...)` / key schedule 逻辑不变
  - 在 full-handshake secrets / peer certificate 已就绪的时点调用新 helper
  - helper fail 时直接设置 `FLastErrorCode` / `FLastErrorString` 并终止握手

**Step 3: Preserve current scope**
- 不实现：
  - CA store / CA file / CA path 驱动的链信任验证
  - resumed PSK 通过 persisted peer certificate 执行 hostname/expiry parity
  - OCSP / CT / revocation runtime parity
- 如果 focused expired-fixture contract 暴露的不是 handshake helper 漏判，而是 `GenerateSigned(...)` 忽略显式 `NotBefore/NotAfter`，允许补一个最小 generator validity fix，但不要顺手扩到更大的 cert-utils 重构。

**Step 4: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Run Focused Regressions And Gate

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Re-run focused and adjacent FreePascal regressions**
- Run:
  - `mkdir -p tmp/freepascal_client_cert_verify_flags_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_cert_verify_flags_runtime -FEtmp/freepascal_client_cert_verify_flags_runtime -otmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime tests/test_freepascal_client_cert_verify_flags_runtime.pas && ./tmp/freepascal_client_cert_verify_flags_runtime/test_freepascal_client_cert_verify_flags_runtime`
  - `mkdir -p tmp/freepascal_client_peer_certificate_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_peer_certificate_surface -FEtmp/freepascal_client_peer_certificate_surface -otmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface tests/test_freepascal_client_peer_certificate_surface.pas && ./tmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface`
  - `mkdir -p tmp/freepascal_client_certificate_flight_requirements && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_certificate_flight_requirements -FEtmp/freepascal_client_certificate_flight_requirements -otmp/freepascal_client_certificate_flight_requirements/test_freepascal_client_certificate_flight_requirements tests/test_freepascal_client_certificate_flight_requirements.pas && ./tmp/freepascal_client_certificate_flight_requirements/test_freepascal_client_certificate_flight_requirements`
  - `mkdir -p tmp/freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption -FEtmp/freepascal_client_session_resumption -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 2: Diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-08-freepascal-client-cert-verify-flags-hostname-expiry-parity.md src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_cert_verify_flags_runtime.pas task_plan.md findings.md progress.md`
- Expected:
  - exit `0`

### Definition Of Done

- FreePascal client full handshake fails on hostname mismatch when `sslVerifyPeer` is enabled and `sslCertVerifyIgnoreHostname` is absent
- FreePascal client full handshake fails on expired peer leaf when `sslVerifyPeer` is enabled and `sslCertVerifyIgnoreExpiry` is absent
- `sslCertVerifyIgnoreHostname` / `sslCertVerifyIgnoreExpiry` can independently suppress those checks at runtime
- focused new regression, adjacent FreePascal regressions, `compile_all_modules.py`, and diff hygiene all pass
- working-memory files record RED/GREEN evidence and the bounded decision to stop before chain trust / OCSP / CT / resumed-session persistence
