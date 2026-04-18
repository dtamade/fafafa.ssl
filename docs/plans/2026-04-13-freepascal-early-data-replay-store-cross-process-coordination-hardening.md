# FreePascal Early-Data Replay-Store Cross-Process Coordination Hardening Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改变 public API、builder/factory/config surface 与 capability wording 的前提下，为 FreePascal TLS 1.3 early-data file-backed anti-replay provider 增加最小跨进程 coordination hardening，避免并发 file-backed acquire 在不同进程间绕过 replay truth。

**Architecture:** 继续复用现有 `TFreePascalFileEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger` 与 backend-private installer seam，不新增新的 provider abstraction。最小修法是在 file-backed provider 内部增加 sidecar lock-file coordination：Unix/Linux 上用 advisory file lock 把 `load -> prune -> replay check -> append -> save` 包在同一份跨进程可见锁里；锁竞争或锁异常时保持 fail closed；孤儿 lock file 本身不构成失败条件。非 Unix 平台保持当前可编译行为，不升级 capability wording。

**Tech Stack:** FreePascal (ObjFPC), `TFileStream`, `Process`, Unix `Unix` advisory file lock, TLS 1.3 early-data focused runtime tests, completeness gate, file-based working memory.

---

## Summary

- 当前 file-backed replay provider 已经收口：
  - persistence / corruption fail-closed / expired-prune
  - backend-private installer seam
  - builder/config/factory parity
  - orphan `.tmp` recovery
- 当前最高 ROI 剩余缺口：
  - provider 仍只用进程内 `TRTLCriticalSection`
  - 不同进程如果同时操作同一 replay store，仍可能在并发窗口里绕过同一份 replay truth
- 本批只做最小 coordination hardening：
  - 增加 sidecar advisory lock
  - 锁竞争时 fail closed
  - orphan `.lock` file 若没有 active lock，不应导致误拒绝
  - 不改 file format、public surface、builder/factory/config surface、capability wording

## Delivery Order

1. 写本轮 plan 与 working-memory 入口。
2. 先在 `tests/test_freepascal_tls13_early_data.pas` 加 RED，锁住 cross-process lock contention fail-closed 合同，并补 orphan lock-file regression contract。
3. 最小 GREEN：只改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
4. 跑 focused regression、focused gate、compile gate、diff hygiene。
5. 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Task 1: RED - Lock cross-process coordination contracts

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Add a lock-holder helper path**
- 在 focused test 中增加最小 helper：
  - 用当前测试可执行文件的 child mode 持有 `AFileName + '.lock'` 上的 advisory lock
  - child 通过 marker file 告知 parent “锁已持有”
  - parent 再执行 provider acquire
- helper 只用于 deterministic RED，不改生产代码

**Step 2: Add lock-contention fail-closed contract**
- 新增 direct provider contract，覆盖：
  - child process 已持有 replay-store lock
  - main replay-store file 还不存在
  - parent process 尝试 acquire fresh valid session
  - provider 必须 fail closed，而不是忽略跨进程锁继续 accept

**Step 3: Add orphan-lock regression contract**
- 新增 direct provider contract，覆盖：
  - sidecar `.lock` 文件存在，但没有 active lock holder
  - provider 仍应允许 fresh valid session acquire
  - fresh acquire 后仍 materialize canonical main replay-store file
- 这个合同防止实现退化成“仅凭 lock-file 存在就拒绝”

**Step 4: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - FAIL，优先失败在“当前 provider 没有跨进程锁协调，lock contention 时仍错误 accept”

### Task 2: GREEN - Add bounded cross-process lock coordination

**Files:**
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Add sidecar lock helpers**
- 在 provider 内部新增最小 private helper，负责：
  - 解析 `FFileName + '.lock'`
  - 获取 / 释放 provider operation 期间的跨进程锁
  - Unix/Linux 上使用 advisory file lock
  - 非 Unix 平台保持可编译 fallback，不扩大行为承诺

**Step 2: Wrap acquire flow under the lock**
- `TryAcquireReplayKey(...)` 继续保持：
  - local critical section
  - load
  - prune
  - replay check
  - append
  - save
- 但在进入真正的 file-store state transition 前，必须先拿到 sidecar lock
- 锁竞争或锁异常 => 返回 `False`

**Step 3: Keep adjacent truth stable**
- orphan `.lock` file 本身不应导致失败；只有 active lock holder 才触发 fail closed
- 继续保持：
  - main file 优先于 orphan `.tmp`
  - corruption / truncated / invalid-version 仍 fail closed
  - canonical main-file temp replace path 不变

**Step 4: Run GREEN**
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
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_replay_store_cross_process_lock_20260413`
  - `python3 scripts/compile_all_modules.py`

**Step 2: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-13-freepascal-early-data-replay-store-cross-process-coordination-hardening.md src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`

### Definition Of Done

- file-backed provider 在 Unix/Linux 上对 replay-store acquire 有跨进程可见的 bounded lock coordination
- lock contention 时 fail closed
- orphan lock-file without active lock holder 不会导致误拒绝
- orphan `.tmp` recovery、corruption fail-closed、canonical main-file write path 继续保持
- focused test、focused gate、compile gate、diff hygiene 都有 fresh evidence
- public API、builder/factory/config surface 与 capability wording 都不变
