# FreePascal TLS 1.3 Early-Data Anti-Replay Hardening Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不新增公开 API 的前提下，把 pure Pascal backend 现有的 TLS 1.3 early-data replay 防护从“单次 ticket 消费”收紧为“基于 session 真值、带过期语义的 bounded process-local anti-replay ledger”，并保持 focused gate 继续覆盖这条 0-RTT 主线。

**Architecture:** 延续当前 early-data transport/policy 路线，不改 `ISSLEarlyDataContext` / `ISSLEarlyDataConnection`。实现继续集中在 FreePascal backend 内部：`TFreePascalSession` 作为 ticket lifetime / timeout 真值源，`TFreePascalContext` 维护 replay ledger，`TFreePascalConnection.DoAccept` 在 resumed early-data path 上通过 session-aware ledger 决定 accept/reject。能力声明仍保持 experimental，只把 anti-replay 文案收紧到真实行为。

**Tech Stack:** FreePascal (ObjFPC), pure Pascal TLS 1.3 session/resumption/early-data units, offline scripted early-data tests, file-based working memory, focused completeness gate.

---

## Summary

- 当前树上已经具备：
  - early-data protocol primitives
  - client/server early-data transport contract
  - process-local replay ledger 雏形
- 当前剩余缺口不是新的 public API，而是 replay ledger 语义过宽：
  - ledger 只按 ticket 字符串记账
  - 不复用 session 有效期真值
  - 不会主动清理过期 replay entry
- 本批只收紧现有内部行为：
  - replay ledger 改为 session-aware acquire
  - replay entry 记录 `ticket key + expires at`
  - acquire 前先 prune expired entry
  - expired session 不得占用或污染 replay ledger
  - capability wording 保持 `experimental`

## Delivery Order

1. 写 plan 与 working-memory 入口。
2. 先在 `tests/test_freepascal_tls13_early_data.pas` 加 RED，覆盖 replay ledger 过期/去污染 contract。
3. 最小实现只改 `src/fafafa.ssl.freepascal.session.pas`、`src/fafafa.ssl.freepascal.context.pas`、`src/fafafa.ssl.freepascal.connection.pas`。
4. 跑 focused regressions 与 completeness gate。
5. 回填 findings / progress / task plan。

### Task 1: Add Replay-Ledger RED Contracts

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Add direct replay-ledger contract**
- 增加针对 `IFreePascalEarlyDataReplayLedger` 的 focused contract：
  - valid resumable session 第一次 acquire 成功
  - 同 session 第二次 acquire 失败
  - expired session acquire 失败
  - expired acquire 不得污染 ledger；fresh valid session 仍可成功 acquire

**Step 2: Keep end-to-end server replay path**
- 保留现有：
  - accepted early-data path => accepted + readable bytes
  - replay path => rejected + resumed handshake still succeeds

**Step 3: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - FAIL，集中暴露：
    - replay ledger 无法直接按 session 真值拒绝 expired session
    - expired acquire 与 valid acquire 的区别还未在 ledger 层体现

### Task 2: Implement Session-Aware Replay Ledger

**Files:**
- Modify: `src/fafafa.ssl.freepascal.session.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Make internal replay interface session-aware**
- 在 `src/fafafa.ssl.freepascal.session.pas`：
  - 把 `IFreePascalEarlyDataReplayLedger` 改成接收 `ISSLSession`
  - 不新增公开接口

**Step 2: Add bounded replay entries with expiry**
- 在 `src/fafafa.ssl.freepascal.context.pas`：
  - 用结构化 replay entry 替代 `array of string`
  - entry 至少保存：
    - ticket key
    - expires at
  - acquire 前先 prune 过期 entry
  - 过期时间必须复用当前 session 真值：
    - 以 `TFreePascalSession.IsValid` / timeout / ticket lifetime 的现有 contract 为准
  - `SetSessionCacheMode(False)` / `SetSessionCacheSize(...)` 继续同步影响 ledger

**Step 3: Rewire server accept path**
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - resumed early-data accept 判定改为把 cached session 传给 replay ledger
  - accept 条件保持：
    - positive `max_early_data_size`
    - `sslEarlyDataServerAccept`
    - replay ledger acquire success
  - reject 后握手继续成功，状态保持 `sslEarlyDataRejected`

**Step 4: Run GREEN**
- Re-run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - PASS

### Task 3: Run Focused Regressions And Capability Wording Checks

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas` (only if wording assertion needs tightening)
- Modify: `tests/test_capability_cache.pas` (only if wording assertion needs tightening)
- Modify: `src/fafafa.ssl.freepascal.lib.pas` (only if KnownIssues wording is updated)

**Step 1: Keep capability truth stable**
- `ZeroRTTSupport` / `EarlyDataSupport` 继续保持 `sslSupportExperimental`
- `KnownIssues` 只允许收紧成真实实现表述：
  - bounded process-local anti-replay ledger
  - 仍无 cross-process/distributed coordination

**Step 2: Run focused regressions**
- Run:
  - `mkdir -p tmp/freepascal_server_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_server_session_resumption -FEtmp/freepascal_server_session_resumption -otmp/freepascal_server_session_resumption/test_freepascal_server_session_resumption tests/test_freepascal_server_session_resumption.pas && ./tmp/freepascal_server_session_resumption/test_freepascal_server_session_resumption`
  - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_antireplay_20260408`
- Expected:
  - PASS

### Task 4: Write Back Working Memory And Diff Hygiene

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record findings**
- 记录：
  - replay ledger 已改为 session-aware
  - replay entry 具备过期语义
  - expired session 不会污染 ledger
  - server accept path 继续以 bounded process-local policy 做 accept/reject

**Step 2: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-08-freepascal-tls13-early-data-anti-replay-hardening.md src/fafafa.ssl.freepascal.session.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.connection.pas src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_tls13_early_data.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas task_plan.md findings.md progress.md`
- Expected:
  - exit `0`
