# FreePascal Early-Data File-Backed Runtime Crash, Lock, And Stress Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改 public API、builder/factory/config surface、`TFreePascalContext` / `TFreePascalConnection` wiring 与 capability wording 的前提下，为 FreePascal early-data file-backed anti-replay opt-in 路径补齐下一批最高 ROI 的 runtime durability evidence：crash-window recovery、runtime lock-contention fail-closed、以及一个最小 restart stress smoke。

**Architecture:** 继续复用现有 backend-private file-backed installer seam、scripted resumed early-data runtime harness、child self-exec helper、session serialize/deserialize helper，以及现有 file-backed provider 的 orphan-temp / sidecar-lock 行为。第一优先级仍是 tests-first：先在 `tests/test_freepascal_tls13_early_data.pas` 加 focused runtime contracts，把 runtime truth 锁死；只有 fresh RED 明确指向 provider 内部缺口时，才最小查看并修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data scripted runtime tests, backend-private installer seam, file-backed replay-store provider, child-process self-exec, file-based working memory.

---

## Summary

- 当前已具备的 live truth：
  - file-backed provider 的 direct persistence / corruption / orphan-temp / cross-process lock / cross-context contracts 已存在
  - installer/runtime restart durability 也已有一条 fresh contract
- 当前最高 ROI 的剩余批次不是再扩面，而是：
  - 把 orphan-temp / lock-contention 这两条 provider truth 再接到真实 resumed early-data `Accept` path
  - 用一个最小 restart stress smoke 防止 replay truth / fresh truth 在 repeated restart 后漂移
- 本批明确不做：
  - capability wording 升级
  - distributed persistence
  - public API / builder / config 新扩面
  - `context.pas` / `connection.pas` 结构调整

## Delivery Order

1. 写本轮 plan 与 working-memory 入口。
2. 在 `tests/test_freepascal_tls13_early_data.pas` 先补 P0 crash-window runtime contract。
3. 在同一 focused test 文件补 P0 runtime lock-contention fail-closed contract。
4. 若仍保持低风险，再补 P1 small restart stress smoke 与必要 helper cleanup。
5. 跑 focused test；只有 fresh RED 明确指向 provider 缺口时，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
6. 跑 adjacent regressions、completeness gate、compile gate、diff hygiene。
7. 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Task 1: RED - Lock Crash-Window Runtime Recovery

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add a runtime orphan-temp / crash-window helper shape**
- 在 focused test 文件里复用现有：
  - `BuildReplayProviderStoreFilePath(...)`
  - `CleanupReplayProviderStoreFiles(...)`
  - `WriteBytesToFile(...)`
  - `ReadBytesFromFile(...)`
  - `RunReplayProviderRuntimeReplayProbeMode(...)`
  - existing orphan-temp store helpers
- 最小新增或复用 helper，负责把已 materialize 的 replay store 切成：
  - main store 缺失
  - `.tmp` orphan store 保留 replay truth

**Step 2: Add a runtime crash-window contract**
- 新增 focused runtime contract，覆盖：
  - 父进程 installer path 先 accept 一次 resumed early-data
  - replay-store truth 已 materialize
  - 测试把 main store 移成 orphan `.tmp`，模拟 crash-window recovery shape
  - 新进程复用同一个 file-backed path 与同一 serialized session 后，真实 resumed early-data 仍必须 replay reject
  - fresh resumed session 仍 accept

**Step 3: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - 若 runtime orphan-temp recovery 与 real accept path 未接稳，优先 RED
  - 若 provider 当前已天然满足，则直接 GREEN

### Task 2: RED - Lock Runtime Lock-Contention Fail-Closed

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Reuse lock-holder child mode**
- 复用现有：
  - `TEST_REPLAY_PROVIDER_LOCK_HOLDER_MODE`
  - `RunReplayProviderLockHolderMode(...)`
  - `HandleReplayProviderChildMode`
  - marker file helpers

**Step 2: Add a runtime lock-contention contract**
- 新增 focused runtime contract，覆盖：
  - 父进程持有可 replay 的 resumable session
  - 另一个 child 进程先持有 sidecar advisory lock
  - 当前进程走真实 resumed early-data `Accept` path
  - 断言：
    - 握手继续成功
    - session reused
    - early-data rejected
    - accepted signal suppressed
    - discarded bytes unreadable
  - 释放 lock 后，再用 fresh resumed session 断言 early-data 仍 accept

### Task 3: P1 - Add A Tiny Restart Stress Smoke

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Keep it tiny**
- 只在不明显增加 brittle risk 时实现：
  - 2 到 3 次 restart / probe loop
  - 每轮都验证：
    - replayed session reject
    - fresh session accept
  - 不引入 timing-sensitive sleeps
  - 不引入大规模随机化

**Step 2: Helper cleanup**
- 若需要，只做最小 test-helper 收敛：
  - child-mode dispatch clearer
  - session file / marker cleanup 更稳
  - 不做 unrelated test refactor

### Task 4: GREEN - Only If RED Proves A Real Production Gap

**Files:**
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Keep production scope minimal**
- 仅当 fresh RED 明确指向 provider 内部而不是 test harness：
  - 优先只改 file-backed store / provider
  - 不先碰 `context.pas` / `connection.pas`
  - 不改 public API、builder/factory/config surface

### Task 5: Verify Adjacent Truth Stays Locked

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
- Reference: `tests/test_freepascal_backend_basic.pas`
- Reference: `tests/test_capability_cache.pas`

**Step 1: Re-run focused and adjacent verification**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_file_backed_runtime_crash_lock_stress_20260414`
  - `python3 scripts/compile_all_modules.py`

**Step 2: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-file-backed-runtime-crash-lock-and-stress.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`

### Definition Of Done

- file-backed installer/runtime path 获得 fresh crash-window recovery runtime evidence
- runtime lock-contention fail-closed truth 被锁到真实 resumed early-data accept path
- 若 tiny restart stress 实现，则它稳定证明 replay truth / fresh truth 不漂移
- 若 runtime contracts 天然为 GREEN，则本批不引入任何 production code 修改
- capability wording、public surface、context/connection wiring 都不变
- focused tests、adjacent regressions、completeness gate、compile gate、diff hygiene 都有 fresh evidence
