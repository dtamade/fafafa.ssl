# FreePascal Early-Data Store Runtime Fail-Closed Closeout Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改变 public API、builder/factory/config surface 与 capability wording 的前提下，先把 `InstallStoreBackedReplayLedger(...)` 接入的真实 resumed early-data runtime fail-closed 合同补成 fresh evidence；如果这条 runtime 合同已经天然成立，则顺势用同一批次补一个 backend-private shared in-memory replay-store prototype，证明新的 store seam 不只服务于 file-backed 实现。

**Architecture:** 继续复用已经稳定的 `IFreePascalEarlyDataReplayStore`、`TFreePascalStoreBackedEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger`、`IFreePascalContextEarlyDataReplayProviderInstaller` 与 `TFreePascalConnection.DoAccept` resumed early-data accept path。第一优先级是补 runtime RED，锁住 store guard/load/save 异常或 `False` 返回时，真实握手路径必须 fail closed 为 `sslEarlyDataRejected`，而不是冒异常或 silent accept。若 fresh runtime RED 已直接是 GREEN，则不强行改现有行为，而是在 `earlydatareplay` 单元里补一个 backend-private shared in-memory store 实现，继续通过现有 `InstallStoreBackedReplayLedger(...)` / provider-backed ledger wiring 接到 runtime。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 resumed early-data runtime contracts, backend-private replay-store seam, focused test binary, completeness gate, file-based working memory.

---

## Summary

- 当前 live truth：
  - internal replay-store seam 已存在
  - generic store-backed provider 已统一承接 replay acquire 语义
  - context active-ledger seam 与 resumed accept wiring 已稳定
  - direct store failure fail-closed focused tests 已覆盖 provider/ledger 边界
- 当前最高 ROI 剩余 closeout 只在：
  - `InstallStoreBackedReplayLedger(...)` 接入的真实 resumed early-data runtime path 是否也有 fresh fail-closed evidence
- 若 runtime path 已经天然满足：
  - 不伪造“必须修代码”的结论
  - 直接把剩余批次价值投到第二个真实 store implementation prototype
- 本批明确不做：
  - public API / builder / factory / config 扩面
  - capability wording 升级
  - distributed / cross-host anti-replay

## Delivery Order

1. 新建本轮 plan，并在 working-memory 中登记目标、条件分支与验证标准。
2. 先在 `tests/test_freepascal_tls13_early_data.pas` 增加 runtime fail-closed RED，锁住 store guard/load/save failure modes 经过 `InstallStoreBackedReplayLedger(...)` 接入真实 resumed handshake 时的行为。
3. 运行 focused RED。
4. 若 RED 失败：只做最小 GREEN，让真实 runtime path fail closed。
5. 若 RED 已经是 GREEN：不动现有 runtime 逻辑，顺势补 backend-private shared in-memory store prototype 与 focused coverage。
6. 跑 focused regression、focused completeness gate、compile gate、diff hygiene。
7. 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Task 1: RED - Lock Runtime Fail-Closed Store Contracts

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add a runtime fail-closed helper around store failure modes**
- 在 focused test 文件里复用现有：
  - `TSharedReplayEntryStore`
  - `TReplayStoreFailureMode`
  - `InstallStoreBackedReplayLedger(...)`
  - scripted resumed early-data runtime helper
- 新增一个最小 runtime assertion helper，负责：
  - 创建 FreePascal server context
  - 安装指定 failure mode 的 shared replay store
  - capture resumable session
  - 用 resumed early-data 流量走真实 `Accept`
  - 断言：握手成功、session reused、early-data rejected、无 accepted signal、discarded early bytes 不可读

**Step 2: Add runtime fail-closed contracts**
- 覆盖至少以下 modes：
  - `rsfmRaiseOnGuard`
  - `rsfmRaiseOnLoad`
  - `rsfmRaiseOnSave`
  - `rsfmFalseOnGuard`
  - `rsfmFalseOnLoad`
  - `rsfmFalseOnSave`
- 每个 mode 都必须证明：
  - `LConn.Accept` 不冒异常
  - resumed handshake 继续成功
  - `GetEarlyDataStatus = sslEarlyDataRejected`

**Step 3: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - 如果 runtime closeout 还缺口，先出现 RED
  - 如果这条路径早已满足，则直接 GREEN；这应被视为“fresh closeout evidence”，而不是失败

### Task 2A: GREEN - Fix Runtime Fail-Closed Only If Task 1 Is Red

**Files:**
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas` (only if fresh evidence proves needed)
- Modify: `src/fafafa.ssl.freepascal.connection.pas` (only if fresh evidence proves needed)

**Step 1: Apply the smallest runtime-safe fix**
- 仅在 fresh RED 证明 runtime gap 真实存在时修改生产代码
- 目标只限于：
  - store/provider exception 继续 fail closed
  - runtime accept path 不冒异常
  - resumed handshake 继续成功
- 不重开任何新的 abstraction / public surface

**Step 2: Re-run focused test**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 2B: GREEN - Shared In-Memory Store Prototype If Task 1 Is Already Green

**Files:**
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Add a backend-private shared in-memory replay store**
- 在 `src/fafafa.ssl.freepascal.earlydatareplay.pas` 新增一个最小 concrete store：
  - `TFreePascalSharedInMemoryReplayStoreGuard`
  - `TFreePascalSharedInMemoryReplayStore`
- contract 继续只表达：
  - acquire update guard
  - load entries
  - save entries
- store 自己不持有 replay-check 语义

**Step 2: Add focused contracts for the real store class**
- 在 `tests/test_freepascal_tls13_early_data.pas` 用新的真实 store class 替换 test-only helper，补最小 focused coverage：
  - provider rebuild 仍保持 replay truth
  - `InstallStoreBackedReplayLedger(...)` cross-context runtime 仍 reject replay
- 不删除现有 shared test helper，除非真实 store class 能无损替代且不增加额外 churn

**Step 3: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Verify No Adjacent Drift

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run focused verification**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_store_runtime_failclosed_closeout_20260413`
  - `python3 scripts/compile_all_modules.py`

**Step 2: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-13-freepascal-early-data-store-runtime-failclosed-closeout.md src/fafafa.ssl.freepascal.earlydatareplay.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`

### Definition Of Done

- store guard/load/save failure modes 在真实 resumed early-data runtime path 上有 fresh fail-closed evidence
- 若 runtime gap 存在，则已用最小改动修复，不改变 public surface
- 若 runtime gap 已不存在，则已补一个 backend-private second concrete store implementation prototype，证明新 store seam 不只服务于 file-backed 形态
- focused test、focused gate、compile gate、diff hygiene 都有 fresh evidence
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
