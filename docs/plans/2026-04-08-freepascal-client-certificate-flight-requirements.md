# FreePascal Client Certificate Flight Requirements Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收紧 pure Pascal TLS 1.3 client full-handshake 底线：非 PSK resumed 的客户端握手，必须在 `Finished` 之前收到 `Certificate` 与 `CertificateVerify`，不能再接受只含 `EncryptedExtensions + Finished` 的 server flight。

**Architecture:** 这是一个刻意收窄的 validation hardening batch，不尝试在同一批里补完完整链验证、hostname/expiry runtime parity、OCSP/CT 或 peer-certificate public API。当前发现的更大风险是：`TFreePascalConnection.ProcessEncryptedServerFlight(...)` 会在 full handshake 上吞掉 `Certificate` / `CertificateVerify` 类型而不做存在性约束，导致 server 只发 `EncryptedExtensions + Finished` 也能通过。实现策略是先用 focused offline RED 把这个缺口钉住，再在连接层最小补上“full handshake 必须看到这两个消息；resumed PSK 继续允许省略”的 fail-closed contract。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, pure Pascal TLS 1.3 handshake helpers, offline scripted stream fixtures, file-based working memory.

---

## Summary

- 当前 `SetCertVerifyFlags(...)` / `GetCertVerifyFlags(...)` 只停在 context storage 层，还没有 runtime parity。
- 但在继续做 flag parity 前，先暴露出一个更高优先级的验证底线缺口：
  - client `ProcessEncryptedServerFlight(...)` 只显式处理 `EncryptedExtensions` 和 `Finished`
  - full handshake 路径没有要求必须看到 `Certificate` / `CertificateVerify`
  - 现有离线握手脚本正是靠“只发 `EncryptedExtensions + Finished`”让 client 成功
- resumed PSK handshake 合理地可以省略这两个消息，所以这批要保持边界克制：
  - full handshake fail-closed
  - resumed PSK continue-pass
  - 不在这批里扩到证书链 / hostname / expiry / OCSP / CT

## Delivery Order

1. 写 plan 与 working-memory 入口，锁定范围为 FreePascal client certificate flight requirements。
2. 先加 focused RED，证明 full handshake 错误接受了缺失 `Certificate` / `CertificateVerify` 的 server flight，而 resumed PSK 仍应允许。
3. 最小实现只改 `src/fafafa.ssl.freepascal.connection.pas`。
4. 跑 focused tests、相邻 FreePascal 回归、`python3 scripts/compile_all_modules.py`、diff hygiene。
5. 回填 findings / progress / task plan，并把后续 queue 收回到更完整的 validation hardening。

### Task 1: Add RED Coverage For Missing Certificate Flight

**Files:**
- Add: `tests/test_freepascal_client_certificate_flight_requirements.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`
- Reference: `tests/test_freepascal_client_session_resumption.pas`

**Step 1: Write the failing offline client contract**
- 新建 `tests/test_freepascal_client_certificate_flight_requirements.pas`：
  - 复用 scripted stream 思路，驱动真实 `TFreePascalConnection.Connect`
  - case 1: initial/full handshake
    - 服务端脚本只发 `ServerHello + EncryptedExtensions + Finished`
    - 断言 client `Connect = False`
    - 断言错误文本明确指向缺失 `Certificate` / `CertificateVerify`
  - case 2: resumed PSK handshake
    - 继续允许服务端只发 `ServerHello(pre_shared_key) + EncryptedExtensions + Finished`
    - 断言 client `Connect = True`
    - 断言 `IsSessionReused = True`

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_client_certificate_flight_requirements && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_certificate_flight_requirements -FEtmp/freepascal_client_certificate_flight_requirements -otmp/freepascal_client_certificate_flight_requirements/test_freepascal_client_certificate_flight_requirements tests/test_freepascal_client_certificate_flight_requirements.pas && ./tmp/freepascal_client_certificate_flight_requirements/test_freepascal_client_certificate_flight_requirements`
- Expected:
  - RED because current full-handshake client path still accepts server flight without `Certificate` / `CertificateVerify`
  - resumed PSK branch should remain green

### Task 2: Implement Minimal Full-Handshake Floor

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add explicit full-handshake message presence requirements**
- 在 `ProcessEncryptedServerFlight(...)`：
  - 跟踪是否看到 `TLS_HANDSHAKE_TYPE_CERTIFICATE`
  - 跟踪是否看到 `TLS_HANDSHAKE_TYPE_CERTIFICATE_VERIFY`
  - 仅在 full handshake（非 resumed / 非 PSK）上启用要求
  - 如果在未看到两者前收到 `Finished`，fail-closed
  - 如果处理预算耗尽或退出时缺少任一消息，也 fail-closed

**Step 2: Preserve transcript and resumed behavior**
- 保持现有 transcript append 顺序不变
- 不改变 `EncryptedExtensions` / `Finished` 处理逻辑
- resumed PSK path 继续允许 server 省略 `Certificate` / `CertificateVerify`

**Step 3: Run GREEN**
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
  - `mkdir -p tmp/freepascal_client_certificate_flight_requirements && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_certificate_flight_requirements -FEtmp/freepascal_client_certificate_flight_requirements -otmp/freepascal_client_certificate_flight_requirements/test_freepascal_client_certificate_flight_requirements tests/test_freepascal_client_certificate_flight_requirements.pas && ./tmp/freepascal_client_certificate_flight_requirements/test_freepascal_client_certificate_flight_requirements`
  - `mkdir -p tmp/freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption -FEtmp/freepascal_client_session_resumption -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 2: Diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-08-freepascal-client-certificate-flight-requirements.md src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_certificate_flight_requirements.pas task_plan.md findings.md progress.md`
- Expected:
  - exit `0`

### Definition Of Done

- Full-handshake FreePascal client path no longer accepts server flight missing `Certificate` / `CertificateVerify`
- Resumed PSK handshake behavior is preserved
- focused new regression, adjacent FreePascal regressions, and `compile_all_modules.py` pass
- working-memory files record RED/GREEN evidence and the rationale for prioritizing this floor before broader runtime validation parity
