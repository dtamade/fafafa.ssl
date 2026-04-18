# FreePascal Early-Data Directory-Store `.bakdir` Residue Semantics Implementation Plan

**Goal:** 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 directory-backed replay-store 剩余最高 ROI 的 `.bakdir` residue family 直接锁成 focused contracts：canonical replace 成功后若 cleanup 删除 `.bakdir` 失败，fresh replay truth 仍必须落在 canonical `main`；下一次 fresh save 遇到无法预清理的 stale `.bakdir` 时，provider/runtime 都必须 fail closed，并保持现有 replay truth 不漂移。

**Architecture:** 继续严格 tests-first，并完全复用现有 `TFreePascalDirectoryEarlyDataReplayStore`、`TFreePascalStoreBackedEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger`、`BuildReplayProviderStoreDirectoryPath(...)`、`CleanupReplayProviderStoreDirectory(...)`、`BuildDirectoryReplayStoreServerContext(...)`、`AssertResumedEarlyDataAcceptedAtRuntime(...)`、`AssertResumedEarlyDataRejectedAtRuntime(...)` 与 `InstallStoreBackedReplayLedger(...)`。先在 `tests/test_freepascal_tls13_early_data.pas` 镜像 file-backed `.bak` residue 家族补 direct/runtime 合同；只有 fresh RED 明确落到 directory-store cleanup seam 无法脚本化，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，且只允许增加内部 tests-only override seam，不重开 `SaveEntries(...)` 逻辑、public wiring 或 capability wording。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data focused contracts, directory-backed local replay store, store-backed replay seam, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先补一个 scripted directory-store 子类，只对 `<store>.bakdir` 的 cleanup 删除做 deterministic 失败注入。
2. 再补 direct provider 合同，锁住：
   - cleanup delete 失败后 `.bakdir` residue 可见，但 canonical `main` truth 仍生效
   - 下一次 fresh save 遇到 stale `.bakdir` 无法预删时继续 fail closed
3. 再补 real runtime resumed early-data 合同，保证 store-backed runtime `Accept` path 也保持同一语义。
4. focused 跑 fresh RED；如果只是缺内部测试 seam，就做最小 `protected virtual` 暴露，不改 `SaveEntries(...)` 行为。
5. 跑 focused / adjacent / completeness / compile / diff hygiene，再回写 roadmap / working-memory。

## Task 1: RED - Lock Direct `.bakdir` Residue Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Add a scripted cleanup-delete-failure directory store**
- 新增最小 subclass：
  - `TScriptedBackupCleanupDeleteFailureDirectoryReplayStore`
- 约束：
  - 不重写 `SaveEntries(...)`
  - 只在 `RemovePathTree(<store>.bakdir)` 时 deterministic 返回 `False`

**Step 2: Add a direct provider contract**
- 复用现有：
  - `BuildReplayProviderStoreDirectoryPath(...)`
  - `CleanupReplayProviderStoreDirectory(...)`
  - `TFreePascalStoreBackedEarlyDataReplayProvider`
  - `TFreePascalProviderBackedEarlyDataReplayLedger`
- 覆盖：
  - existing replay truth 先 materialize canonical `main`
  - scripted store accept fresh residue session 后，canonical `main` 继续存在、`.tmpdir` cleaned、`.bakdir` residue 保留
  - original / fresh accepted replay truth 立即继续 reject
  - 下一次 blocked fresh session 写入时，因为 stale `.bakdir` 无法预删而 fail closed
  - provider rebuild 后，原 truth 继续 reject、blocked session 恢复 first accept，并消费/清理 stale `.bakdir`

## Task 2: RED - Lock Runtime `.bakdir` Residue Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Add a real runtime contract**
- 复用现有：
  - `BuildDirectoryReplayStoreServerContext(...)`
  - `InstallStoreBackedReplayLedger(...)`
  - `AssertResumedEarlyDataAcceptedAtRuntime(...)`
  - `AssertResumedEarlyDataRejectedAtRuntime(...)`
  - `CaptureServerIssuedSession(...)`
- 覆盖：
  - runtime resumed early-data accept path 在 `.bakdir` cleanup delete 失败后仍 accept fresh truth
  - canonical `main` 保持存在，`.tmpdir` cleaned，`.bakdir` residue 保留
  - original / fresh accepted replay truth 立即继续 reject
  - 下一次 blocked session 在 stale `.bakdir` 无法预删时 early-data reject
  - runtime rebuild 后 stale `.bakdir` 被正常消费，blocked session 恢复 first accept，后续 replay 继续 reject

## Task 3: GREEN - Keep The Fix Bounded

**Files:**
- Modify only if needed: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Add the smallest internal seam only if RED requires it**
- 只有 fresh RED 落到“directory-store cleanup helper 无法 script”时，才允许最小修法：
  - 把 `RemovePathTree(...)` 暴露为 `protected virtual`
  - 不改变 `SaveEntries(...)` 清理 / replace / fail-closed 语义
  - 不新增 public API

## Task 4: Verify And Close Out

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run adjacent verification**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_bakdir_residue_semantics_20260419`
  - `python3 scripts/compile_all_modules.py`
  - `git diff --check -- docs/plans/2026-04-19-freepascal-early-data-directory-store-bakdir-residue-semantics.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas docs/ROADMAP.md task_plan.md findings.md progress.md`

### Definition Of Done

- directory-store cleanup-delete-failure residue semantics are covered by direct and runtime contracts
- successful canonical replace can leave visible stale `.bakdir` residue without losing canonical `main` replay truth
- stale undeletable `.bakdir` on next save keeps provider/runtime fail-closed and preserves existing replay truth
- recovery with a normal store/runtime consumes stale `.bakdir`, re-opens the blocked session, and replay truth remains durable
- focused / adjacent / completeness / compile / diff hygiene all have fresh evidence
- capability wording remains `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
