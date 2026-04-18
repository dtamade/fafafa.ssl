# FreePascal Early-Data Directory-Store Filesystem Blocker Semantics Implementation Plan

**Goal:** 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 directory-backed replay-store 剩余最高 ROI 的 filesystem blocker family 直接锁成 focused contracts：当 canonical `main`、staging `.tmpdir`、或 replace-target `.bakdir` 被 regular file 等错误形态占住时，provider/runtime 都必须 fail closed；同时已有 replay truth 不能被 silent delete、silent replace、或 silent heal，blocker 移除后才允许恢复正常写入。

**Architecture:** 继续严格 tests-first，并完全复用现有 `TFreePascalDirectoryEarlyDataReplayStore`、`TFreePascalStoreBackedEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger`、`BuildDirectoryReplayStoreServerContext(...)`、`AssertResumedEarlyDataAcceptedAtRuntime(...)`、`AssertResumedEarlyDataRejectedAtRuntime(...)`、`BuildReplayProviderStoreDirectoryPath(...)`、`CleanupReplayProviderStoreDirectory(...)` 与 path cleanup helpers。先在 `tests/test_freepascal_tls13_early_data.pas` 镜像 file-backed blocker 风格补 direct/runtime 合同；只有 fresh RED 明确落到 `SaveEntries(...)` 的 path-shape handling，才最小修改 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，不重开 public wiring 或其他 provider family。

**Tech Stack:** FreePascal (ObjFPC), directory-backed local replay store, TLS 1.3 early-data focused runtime tests, backend-private store-backed replay seam, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先补 directory-store direct blocker contracts，覆盖：
   - canonical `main` 被 regular file 占住
   - `.tmpdir` 被 regular file 占住
   - `.tmpdir` / `.bakdir` blocker 在 existing truth update path 上必须 preserve 现有 replay truth
2. 再补 real runtime blocker contracts，保证 resumed early-data `Accept` path 也同样 fail closed。
3. focused 跑 fresh evidence；如果 RED 命中 `dirstore`，只做最小 path-shape fix。
4. 跑 focused / adjacent / completeness / compile / diff hygiene，再回写 roadmap / working-memory。

## Task 1: RED - Lock Direct Filesystem Blocker Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Add a direct provider blocker contract**
- 复用现有：
  - `BuildReplayProviderStoreDirectoryPath(...)`
  - `CleanupReplayProviderStoreDirectory(...)`
  - `RemoveReplayProviderPathIfExists(...)`
  - `TouchFile(...)`
- 覆盖：
  - canonical `main` 路径被 file 占住时，fresh acquire fail closed，且 blocker 不被 silent delete
  - `.tmpdir` 路径被 file 占住时，fresh acquire fail closed，且 canonical `main` 不 materialize
  - canonical replay truth 已存在时，`.tmpdir` / `.bakdir` file blocker 会让 fresh blocked session fail closed，但 original replay truth 继续 reject
  - blocker 移除后，同一 blocked session 才恢复 accept；provider rebuild 后 replay truth 仍成立

**Step 2: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`

## Task 2: RED - Lock Runtime Filesystem Blocker Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Add a real runtime blocker contract**
- 复用现有：
  - `BuildDirectoryReplayStoreServerContext(...)`
  - `AssertResumedEarlyDataAcceptedAtRuntime(...)`
  - `AssertResumedEarlyDataRejectedAtRuntime(...)`
  - `CaptureServerIssuedSession(...)`
  - `TouchFile(...)`
- 覆盖：
  - runtime resumed early-data path 在 `main` / `.tmpdir` wrong-shape blocker 下继续 fail closed
  - runtime update path 在 `.tmpdir` / `.bakdir` wrong-shape blocker 下 preserve existing replay truth
  - blocker artifact 保持可见，canonical `main` 不会被 silent heal
  - 移除 blocker 后 fresh resumed early-data 恢复 accept，后续 replay 继续 reject

## Task 3: GREEN - Keep The Fix Bounded

**Files:**
- Modify only if needed: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Fail closed on wrong-shape blocker paths**
- 只有 fresh RED 明确落到 `SaveEntries(...)` 的 path handling，才最小修复：
  - canonical `main` 若存在但不是目录，不允许被隐式 rename / consume
  - `.tmpdir` / `.bakdir` 若存在但不是目录，不允许被隐式删除后继续写入
  - existing replay truth preserve 优先于 silent cleanup

## Task 4: Verify And Close Out

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run adjacent verification**
- Run:
  - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_filesystem_blockers_20260416`
  - `python3 scripts/compile_all_modules.py`
  - `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-directory-store-filesystem-blocker-semantics.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas docs/ROADMAP.md task_plan.md findings.md progress.md`

### Definition Of Done

- directory-store `main` / `.tmpdir` / `.bakdir` wrong-shape blockers are covered by direct and runtime contracts
- fresh blocked session fail closed does not silently delete blocker artifacts
- existing replay truth is preserved across `.tmpdir` / `.bakdir` blocker failures
- blocker removal re-opens the blocked session and replay truth remains durable after rebuild
- focused / adjacent / completeness / compile / diff hygiene all have fresh evidence
- capability wording remains `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
