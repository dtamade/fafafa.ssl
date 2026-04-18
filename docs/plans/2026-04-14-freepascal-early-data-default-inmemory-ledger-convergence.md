# FreePascal Early-Data Default In-Memory Ledger Convergence Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改变 public API、builder/factory/config surface 与 capability wording 的前提下，把 FreePascal TLS 1.3 early-data 默认 in-memory anti-replay 路径收敛到已经稳定的 replay-store seam，避免继续维护两套 replay acquire / lifecycle 语义。

**Architecture:** 保持 `TFreePascalContext` 的默认 shipped behavior、`IFreePascalEarlyDataReplayLedgerAccess`、`TFreePascalConnection.DoAccept` 与现有 focused runtime contracts 不变。最小修法是不再让 `TFreePascalInMemoryEarlyDataReplayLedger` 自己持有整套 in-memory replay acquire 逻辑，而是把它收敛成一个薄包装：内部使用 `TFreePascalSharedInMemoryReplayStore` + `TFreePascalStoreBackedEarlyDataReplayProvider` + `TFreePascalProviderBackedEarlyDataReplayLedger`。为保住默认路径的 clear / disable / capacity / bounded eviction 语义，只新增一个 backend-private managed store/provider seam，作用范围仅限 `src/fafafa.ssl.freepascal.earlydatareplay.pas`。

**Tech Stack:** FreePascal (ObjFPC), backend-private replay-store seam, optional managed store/provider interfaces, TLS 1.3 focused tests, completeness gate, file-based working memory.

---

## Summary

- 当前 live truth：
  - store seam、store-backed provider、provider-backed ledger、context active-ledger seam 都已经稳定
  - 默认 in-memory 路径仍在 `TFreePascalInMemoryEarlyDataReplayLedger` 内部保留独立 replay / prune / capacity 逻辑
  - shared in-memory store concrete class 已存在，但还没有 default-path 所需的 managed clear / capacity 行为
- 当前最高 ROI 的下一步不是再做新 persistence，而是把默认路径也收敛到现有 seam，减少未来变更面
- 推荐方案：
  - 在 `earlydatareplay.pas` 内新增可选 managed store/provider seam
  - 用 shared in-memory store-backed ledger 覆盖 default in-memory lifecycle / capacity 语义
  - 让 `TFreePascalInMemoryEarlyDataReplayLedger` 成为 store-backed 路径的薄包装，而不是继续保留重复实现
- 本批明确不做：
  - public API / builder / factory / config 扩面
  - capability wording 升级
  - file-backed / callback-backed 语义变更

## Approach Comparison

### Option A: 保留当前默认 ledger 独立实现，只补 shared-store managed 语义
- 优点：表面改动更小
- 缺点：默认路径和 store-backed 路径仍是双轨维护，后续继续返工

### Option B: 直接让 `TFreePascalContext` 默认装配 provider-backed ledger
- 优点：收敛最彻底
- 缺点：需要动 `context.pas` 装配面，风险高于收益

### Option C: 让 `TFreePascalInMemoryEarlyDataReplayLedger` 变成 store-backed 薄包装（推荐）
- 优点：默认路径对外形状完全不变，context 基本不动，重复 replay 逻辑最少
- 缺点：仍保留一个包装类名称，但内部 replay 语义已经统一

## Delivery Order

1. 写本轮 plan，并在 working-memory 里登记目标、边界与推荐方案。
2. 先在 `tests/test_freepascal_tls13_early_data.pas` 增加 RED，锁住 shared in-memory store-backed path 的 managed parity 语义。
3. 运行 focused RED，确认当前 shared in-memory store-backed 直连路径还缺 clear / capacity / bounded eviction 合同。
4. 最小 GREEN：只改 `src/fafafa.ssl.freepascal.earlydatareplay.pas`，新增内部 managed seam，并把默认 in-memory ledger 收敛成 store-backed 薄包装。
5. 跑 focused regression、focused completeness gate、compile gate、diff hygiene。
6. 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Task 1: RED - Lock Shared In-Memory Store-Backed Parity Contracts

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`

**Step 1: Add direct managed-lifecycle contract for shared in-memory store-backed ledger**
- 新增 focused direct contract，覆盖：
  - `TFreePascalSharedInMemoryReplayStore`
  - `TFreePascalStoreBackedEarlyDataReplayProvider`
  - `TFreePascalProviderBackedEarlyDataReplayLedger`
- 行为要求：
  - first acquire 成功
  - `SetEnabled(False)` 后 acquire 失败
  - `SetEnabled(True)` 后，同一 session 应重新成功
  - 这条合同锁住 default in-memory 旧有 clear-on-disable 语义

**Step 2: Add direct capacity / bounded eviction contract**
- 新增 focused direct contract，覆盖：
  - shared in-memory store-backed ledger capacity=`2`
  - acquire session1、session2、session3
  - provider/ledger rebuild 后：
    - oldest replay truth 已被驱逐，session1 应可重新 acquire
    - live tail entries 仍 reject replay
- 这条合同锁住 default in-memory 旧有 bounded eviction 语义

**Step 3: Add context-level parity contract for default ledger**
- 新增 focused default-path contract，覆盖：
  - default replay ledger 在 `SetSessionCacheMode(False)` 后拒绝 acquire
  - 重新 `SetSessionCacheMode(True)` 后，同一 session 可重新 acquire
  - `SetSessionCacheSize(0)` 后拒绝 acquire
  - `SetSessionCacheSize(8)` 后可重新 acquire
- 这条测试可以是 GREEN-only parity evidence；真正 RED 预期来自 Step 1 / Step 2

**Step 4: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - RED，优先失败在 shared in-memory store-backed path 还没有 clear / capacity / bounded eviction managed semantics

### Task 2: GREEN - Converge Default In-Memory Ledger To The Store Seam

**Files:**
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Add a backend-private managed store/provider seam**
- 只在 `earlydatareplay.pas` 内新增 optional internal interfaces：
  - managed replay-store contract，至少表达：
    - `Clear`
    - `SetCapacity`
  - managed replay-provider contract，作为 store-backed provider 的薄转发层
- 不扩 `session.pas` public/internal shared surface，除非 fresh evidence 证明必须外提

**Step 2: Teach shared in-memory store capacity + clear semantics**
- 在 `TFreePascalSharedInMemoryReplayStore`：
  - 增加 capacity state
  - `SaveEntries(...)` 后做 bounded eviction
  - `SetCapacity(0)` 清空并禁用 retained entries
  - `Clear` 清空 retained entries
- store 仍然不负责 replay-check

**Step 3: Teach provider-backed ledger to honor managed provider hooks**
- 在 `TFreePascalProviderBackedEarlyDataReplayLedger`：
  - `Clear` 对 managed provider 生效
  - `SetEnabled(False)` 时清空 managed provider-backed transient state
  - `SetCapacity(...)` 时把 capacity 同步给 managed provider
- file-backed / callback-backed / non-managed providers 保持当前 no-op 语义

**Step 4: Collapse default in-memory ledger into a thin wrapper**
- 让 `TFreePascalInMemoryEarlyDataReplayLedger`：
  - 内部装配 `TFreePascalSharedInMemoryReplayStore`
  - 内部装配 `TFreePascalStoreBackedEarlyDataReplayProvider`
  - 内部装配 `TFreePascalProviderBackedEarlyDataReplayLedger`
  - 对外仍实现 `IFreePascalManagedEarlyDataReplayLedger`
- 目标是保留当前 context 装配点不变，但 replay 语义只走一条路径

**Step 5: Run GREEN**
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
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_default_inmemory_ledger_convergence_20260414`
  - `python3 scripts/compile_all_modules.py`

**Step 2: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-default-inmemory-ledger-convergence.md src/fafafa.ssl.freepascal.earlydatareplay.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`

### Definition Of Done

- default in-memory path 的 replay acquire / prune / bounded eviction 语义不再保留独立实现
- `TFreePascalInMemoryEarlyDataReplayLedger` 已收敛成 store-backed 薄包装，或等价最小结构
- shared in-memory store-backed direct contracts 覆盖 disable / capacity / bounded eviction parity
- default context-level parity contracts 继续为 GREEN
- public API、builder/factory/config surface 与 capability wording 都不变
- focused tests、focused gate、compile gate、diff hygiene 都有 fresh evidence
