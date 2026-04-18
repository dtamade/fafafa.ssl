# FreePascal Client OCSP Required Verify-None Boundary Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把 `ssoRequireOCSPStapling` 在 `verify-none` client full-handshake 路径上的边界契约补齐，确保关闭 `sslVerifyPeer` 时不会被 required OCSP fail-closed 误伤。

**Architecture:** 这批只处理 `verify-none + required OCSP` 的 enforcement boundary，不改 OCSP request trigger，不改 optional surface，也不扩到 online fetch / verifier hardening。先在现有 scripted OCSP runtime harness 上写一个 focused RED，若 RED 成立，只在 `ValidateClientOCSPStapling` 增加 `sslVerifyPeer` guard，让 required-policy 只在真实 peer-verification path 生效。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `ISSLOCSPStapling`, scripted TLS 1.3 OCSP stapling runtime test harness, file-based working memory.

---

## Task 1: RED - Add the missing verify-none boundary contract

**Files:**
- Modify: `tests/test_freepascal_client_ocsp_stapling_runtime.pas`

**Step 1: Extend the test harness with verify-mode control**
- 把现有 `NewClientContext(...)` 扩成可传入 `TSSLVerifyModes`：
  - 默认仍保持 `verify-peer`
  - 允许 focused case 传 `[]`
- 保持当前 OCSP option wiring 不变：
  - `ARequireStapling=True` 仍会同时打开 `ssoEnableOCSPStapling`

**Step 2: Add the focused verify-none contract**
- 新增一个单独测试，例如：
  - `TestRequiredStaplingIsIgnoredWhenVerifyPeerDisabled`
- 场景：
  - `SetVerifyMode([])`
  - 打开 `ssoRequireOCSPStapling`
  - scripted server 不返回 stapled OCSP response
- 断言：
  - `Connect = True`
  - `ObservedStatusRequest = True`
  - `ISSLOCSPStapling.GetOCSPResponse = []`
  - `ISSLOCSPStapling.IsOCSPResponseVerified = False`
- 这批刻意不把 request trigger 改成 inert，继续锁住“只跳过 enforcement，不改 request plumbing”。

**Step 3: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime`
- Expected:
  - FAIL because current `ValidateClientOCSPStapling` still enforces required policy even when `sslVerifyPeer` is disabled

## Task 2: GREEN - Add the smallest verify-mode guard

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Skip OCSP enforcement when verify-peer is disabled**
- 在 `ValidateClientOCSPStapling` 里读取 `FContext.GetVerifyMode`
- 最小补一条 guard：
  - `not (sslVerifyPeer in LVerifyMode)` => `Exit(True)`
- 保持其余行为不变：
  - 不改 `ProbeServerHello(...)` 的 `status_request` 触发条件
  - 不改 verify-peer/full-handshake required 失败语义
  - 不改 resumed guard
  - 不改 OCSP state surface

**Step 2: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

## Task 3: Verification And Closeout

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Commands:**
```bash
mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_ocsp_stapling_runtime \
  -FEtmp/freepascal_client_ocsp_stapling_runtime \
  -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime \
  tests/test_freepascal_client_ocsp_stapling_runtime.pas && \
./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime
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
git diff --check -- \
  docs/plans/2026-04-09-freepascal-client-ocsp-required-verifynone-boundary.md \
  src/fafafa.ssl.freepascal.connection.pas \
  tests/test_freepascal_client_ocsp_stapling_runtime.pas \
  task_plan.md findings.md progress.md
```

## Definition Of Done

- `verify-none + ssoRequireOCSPStapling` 不再 fail-closed
- 现有 OCSP request trigger 仍保持不变
- verify-peer/full-handshake required 语义和 resumed boundary 不回退
- focused OCSP runtime contract、session resumption regression、`compile_all_modules.py`、diff hygiene 全绿

## Execution Result

- RED 由 `tests/test_freepascal_client_ocsp_stapling_runtime.pas` 的
  `TestRequiredStaplingIsIgnoredWhenVerifyPeerDisabled` 复现：
  - 当前 `verify-none + ssoRequireOCSPStapling` 仍会被 required-policy fail-closed
- 最小生产修复只改了 `src/fafafa.ssl.freepascal.connection.pas`：
  - 在 `ValidateClientOCSPStapling` 增加 `sslVerifyPeer` guard
  - `sslVerifyPeer` 未启用时直接 `Exit(True)`
- 刻意没有改：
  - `ProbeServerHello(...)` 的 `status_request` request trigger
  - verify-peer/full-handshake 的 required 失败语义
  - resumed guard
  - OCSP verifier / state surface

## Final Verification

- `mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime` => PASS
- `mkdir -p tmp/freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption -FEtmp/freepascal_client_session_resumption -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-client-ocsp-required-verifynone-boundary.md src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_client_ocsp_stapling_runtime.pas task_plan.md findings.md progress.md` => PASS
