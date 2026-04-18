# FreePascal Early-Data Callback Provider Fail-Closed And Durable Shape Validation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改变 public API、builder/factory/config surface 与 capability wording 的前提下，为 FreePascal TLS 1.3 early-data callback/provider 路径补齐 fail-closed 异常语义，并验证 shared callback-owned replay truth 在 provider / ledger 重建后仍成立。

**Architecture:** 继续复用现有 `IFreePascalEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger`、`IFreePascalContextEarlyDataReplayProviderInstaller` 与 resumed early-data accept path，不新增 public abstraction，也不重开 file-backed/provider persistence 设计。本批优先用 focused RED 锁住两类真实边界：provider/callback 抛异常时必须 fail closed；shared callback-owned store object 在 provider / ledger 重建后仍能保持 replay truth。默认最小 GREEN 是把 provider 调用的异常边界收口在现有 replay unit 内，让 early-data reject 而不是把握手打断。

**Tech Stack:** FreePascal (ObjFPC), internal replay provider seam, callback-backed provider helper, TLS 1.3 focused runtime tests, completeness gate, file-based working memory.

---

## Summary

- 当前已稳定收口的真值：
  - replaceable replay-ledger seam
  - provider-backed ledger prototype
  - file-backed provider prototype + lifecycle seam + cross-process hardening
  - builder/config/factory parity
  - backend-private generic replay-provider installer seam
- 当前最高 ROI 的下一步不是再造一个更重的 persistence implementation，而是先把 generic provider seam 的 fail-closed 语义补硬：
  - callback/provider 抛异常时，不应把 resumed handshake 打断
  - early-data 必须 reject，且 discarded early bytes 不可读
  - shared callback-owned replay truth 在 provider / ledger 重建后仍应保持
- 本批明确不做：
  - public provider API
  - distributed / cross-host replay coordination
  - capability wording 升级
  - 文档外扩

## Delivery Order

1. 写本轮 plan 与 working-memory 入口。
2. 先在 `tests/test_freepascal_tls13_early_data.pas` 加 RED，锁住 callback/provider exception fail-closed 与 shared callback-store rebuild shape。
3. 最小 GREEN：默认只改 `src/fafafa.ssl.freepascal.earlydatareplay.pas`。
4. 跑 focused regression、focused completeness gate、compile gate、diff hygiene。
5. 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Task 1: RED - Lock callback/provider fail-closed and rebuild shape contracts

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add an exploding callback-store helper**
- 在 focused test 文件里新增一个最小 helper object：
  - 对外暴露 `TryAcquireReplayKey(...)`
  - 被调用时直接抛出异常
- 不改现有 shared replay-store helper。

**Step 2: Add a direct provider-backed fail-closed contract**
- 新增 focused direct-ledger contract，覆盖：
  - 用 exploding callback 创建 `TFreePascalCallbackEarlyDataReplayProvider`
  - 再用该 provider 创建 `TFreePascalProviderBackedEarlyDataReplayLedger`
  - 对 fresh valid session 调用 `TryAcquireEarlyDataSession(...)`
  - 预期：返回 `False`，且不向测试抛异常

**Step 3: Add a runtime fail-closed contract**
- 新增 focused runtime contract，覆盖：
  - FreePascal server context 安装 exploding callback-backed replay provider
  - 先完成一次 full handshake 并 capture resumable session
  - 随后发起 resumed early-data attempt
  - 预期：
    - `Accept` 仍成功
    - `IsSessionReused` 仍为 `True`
    - `GetEarlyDataStatus = sslEarlyDataRejected`
    - `ObservedServerAcceptedEarlyData = False`
    - `Read(...)` 读不到 discarded early bytes
  - 若握手抛异常，RED 应明确暴露当前 provider exception 未被 fail-close

**Step 4: Add a shared callback-store rebuild-shape contract**
- 新增 focused direct-ledger contract，覆盖：
  - `provider1` / `ledger1` 使用 shared callback-owned store object，first acquire 成功
  - 新建 `provider2` / `ledger2`，但继续指向同一个 store object
  - 同一 session 在 `ledger2` 上 acquire 失败
  - expired session 仍在进入 provider 前被 reject
  - fresh valid session 仍可成功 acquire

**Step 5: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - FAIL，优先暴露 callback/provider 异常会直接打断 acquire 或 resumed early-data handshake

### Task 2: GREEN - Fail close provider exceptions with the smallest safe change

**Files:**
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Add a bounded provider exception boundary**
- 在 provider-backed replay 路径里新增最小异常边界：
  - provider / callback 抛异常时返回 `False`
  - 不让异常从 anti-replay acquire 路径继续冒泡到 resumed early-data accept path
- 不改变 replay key / expires-at 真值解析。

**Step 2: Keep callback helper and installer behavior stable**
- 继续保持：
  - `InstallReplayProviderBackedLedger(...)` / `InstallCallbackBackedReplayLedger(...)` 对 `nil` / missing seam fail closed
  - session-cache enabled / capacity sync 继续通过 context 现有 managed-ledger wiring 生效
  - file-backed provider / generic installer seam / builder/config/factory parity 都不改

**Step 3: Re-run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Verify adjacent truth stays locked

**Files:**
- Reference: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Run focused verification**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_callback_provider_failclosed_20260413`
  - `python3 scripts/compile_all_modules.py`

**Step 2: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-13-freepascal-early-data-callback-provider-failclosed-and-durable-shape-validation.md src/fafafa.ssl.freepascal.earlydatareplay.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`

### Definition Of Done

- callback/provider exception 不会再打断 resumed early-data handshake
- provider-backed replay acquire 在异常场景下 fail closed 返回 `False`
- shared callback-owned replay truth 在 provider / ledger 重建后仍有 fresh coverage
- focused test、focused gate、compile gate、diff hygiene 都有 fresh evidence
- public API、builder/factory/config surface 与 capability wording 都不变
