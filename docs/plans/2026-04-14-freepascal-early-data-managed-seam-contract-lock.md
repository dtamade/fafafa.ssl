# FreePascal Early-Data Managed Seam Contract Lock Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改变 public API、builder/factory/config surface、`TFreePascalContext` / `TFreePascalConnection` wiring 与 capability wording 的前提下，把新引入的 managed replay seam 边界锁成 focused contracts，并补最小 internal-only 注释，减少后续误用与返工。

**Architecture:** 保持默认 shipped behavior 与现有 replay-store seam 完全不变；本批只围绕 `src/fafafa.ssl.freepascal.earlydatareplay.pas` 的 managed seam 做 closeout。测试层新增三类 contract：shared in-memory managed clear/capacity 只作用于默认 seam、non-managed callback/file-backed providers 不会因本地 disable/reenable 或 capacity toggle 被隐式 wipe、managed hook exception 继续被 provider-backed ledger bounded swallow 且 local gate 语义保持确定。若 focused RED 已经天然为 GREEN，则生产层只做 boundary comments，不伪造行为改动。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data focused tests, backend-private replay/provider seam, file-backed replay provider, file-based working memory.

---

## Summary

- 当前 live truth：
  - default in-memory ledger 已经收敛成 shared in-memory store-backed thin wrapper
  - managed clear / capacity 语义当前只保证 shared in-memory seam
  - file-backed / callback-backed providers 仍保持既有 non-managed no-op lifecycle contracts
  - provider-backed ledger 对 managed hook exception 继续 swallow
- 当前最高 ROI 的下一步不是再改 context / connection / public surface，而是：
  - 把这些边界写成 focused regression
  - 在实现里补 internal-only 注释，降低后续误用概率
- 本批明确不做：
  - distributed / cross-host anti-replay
  - default durability 升级
  - capability wording 升级
  - public/internal installer seam 扩面

## Delivery Order

1. 在 focused tests 里先补 seam-boundary RED / fresh evidence。
2. 跑 focused test，确认当前边界是天然 GREEN 还是还存在 drift。
3. 只在 `src/fafafa.ssl.freepascal.earlydatareplay.pas` 做最小注释/边界收口；若 fresh RED 暴露真实 drift，再做最小 GREEN。
4. 跑 focused regression、capability wording regressions、completeness gate、compile gate、diff hygiene。
5. 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Task 1: RED - Lock Managed Seam Boundary Contracts

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Add a non-managed callback/provider lifecycle contract**
- 新增 focused direct contract，覆盖：
  - stateful callback-backed provider / provider-backed ledger
  - first acquire 成功
  - `SetEnabled(False)` 后 acquire 失败（local gate）
  - `SetEnabled(True)` 后，同一 session 仍应 replay reject
  - `SetCapacity(0)` / `SetCapacity(8)` 前后也不应 wipe shared callback replay truth
- 这条合同锁住：managed clear/capacity 不应外溢到 non-managed callback seam。

**Step 2: Add a file-backed lifecycle persistence contract**
- 新增 focused direct contract，覆盖：
  - file-backed provider / provider-backed ledger
  - first acquire 成功
  - ledger disable / re-enable 后，同一 session 仍 replay reject
  - capacity `0 -> 8` 恢复后，同一 session 仍 replay reject
- 这条合同锁住：file-backed persisted truth 不应被 local managed lifecycle toggle 隐式清掉。

**Step 3: Add a managed-hook exception swallow contract**
- 新增 focused direct contract，覆盖：
  - 一个 test-only provider 同时实现：
    - `IFreePascalEarlyDataReplayProvider`
    - `IFreePascalManagedReplayProvider`
  - managed `Clear` / `SetCapacity` 故意抛异常
  - `SetEnabled(False/True)` 与 `SetCapacity(0/8)` 都不应向外抛异常
  - local gate 语义继续有效：
    - disabled / zero-capacity => reject
    - restored gate => provider 调用恢复可达
- 这条合同锁住：当前 swallow 语义是 deliberate boundary，而不是偶然行为。

**Step 4: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - 如果当前 boundary 已正确，可能直接 GREEN；否则失败点应精确落在 managed seam 外溢或 exception boundary drift。

### Task 2: GREEN / CLOSEOUT - Minimal Boundary Documentation Or Fix

**Files:**
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Mark managed seam as internal-only and scope-bound**
- 在 `IFreePascalManagedReplayStore` / `IFreePascalManagedReplayProvider` 附近补注释：
  - internal-only managed seam
  - 当前只保证 shared in-memory lifecycle parity
  - file-backed / callback-backed providers 不要求实现该 contract

**Step 2: Clarify default wrapper convergence**
- 在 `TFreePascalInMemoryEarlyDataReplayLedger` 附近补注释：
  - 该类型现在只是 default shipped wrapper
  - retained replay truth 实际落在 shared in-memory store-backed path

**Step 3: Clarify deliberate swallow boundary**
- 在 `TFreePascalProviderBackedEarlyDataReplayLedger.Clear` / `SetCapacity` / `SetEnabled(False)` 相关路径补注释：
  - managed hook failure 不应打断 handshake/runtime path
  - local enabled/capacity gate 仍然是 authoritative truth
- 若 Task 1 fresh RED 暴露真实 drift，则在同一文件内做最小行为修复；否则只做注释 closeout。

**Step 4: Re-run focused test**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Verify Adjacent Truth Stays Locked

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
- Reference: `src/fafafa.ssl.freepascal.lib.pas`
- Reference: `tests/test_freepascal_backend_basic.pas`
- Reference: `tests/test_capability_cache.pas`

**Step 1: Re-run capability wording regressions**
- Run:
  - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
- Expected:
  - PASS
  - `KnownIssues` 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`

**Step 2: Run focused gate + compile gate**
- Run:
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_managed_seam_contract_lock_20260414`
  - `python3 scripts/compile_all_modules.py`

**Step 3: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-managed-seam-contract-lock.md src/fafafa.ssl.freepascal.earlydatareplay.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`

### Definition Of Done

- shared in-memory managed seam、non-managed callback seam、file-backed seam 的 lifecycle boundary 都有 fresh focused evidence
- managed hook exception swallow boundary 有 explicit regression evidence
- `src/fafafa.ssl.freepascal.earlydatareplay.pas` 已补 internal-only / scope-bound 注释，且无多余行为扩面
- `TFreePascalContext` / `TFreePascalConnection`、public API、builder/factory/config surface 与 capability wording 都不变
- focused tests、capability wording tests、completeness gate、compile gate、diff hygiene 都有 fresh evidence
