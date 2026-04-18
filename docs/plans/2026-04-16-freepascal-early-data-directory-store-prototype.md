# FreePascal Early-Data Directory Store Prototype Implementation Plan

**Goal:** 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 FreePascal TLS 1.3 early-data anti-replay 增加第二个 backend-private 单机可控持久化 concrete store：directory-backed replay store prototype，并用 direct/runtime focused contracts 证明现有 `IFreePascalEarlyDataReplayStore` seam 已经足够承载第二种本地持久化形态。

**Architecture:** 继续优先走 tests-first，并完全复用现有 `IFreePascalEarlyDataReplayStore`、`TFreePascalStoreBackedEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger` 与 `InstallStoreBackedReplayLedger(...)` 接线。新增一个独立单元 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，只实现 backend-private store contract：用 sidecar `.lock` 做 update-guard，用 `<store>/*.entry` 承载 replay truth，用 `<store>.tmpdir` / `<store>.bakdir` 做整目录 replace；不新增新的 public helper，也不重开 file-backed `.bak` family。

**Tech Stack:** FreePascal (ObjFPC), `IFreePascalEarlyDataReplayStore`, `TFreePascalStoreBackedEarlyDataReplayProvider`, directory-backed local persistence, TLS 1.3 scripted runtime tests, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先在 `tests/test_freepascal_tls13_early_data.pas` 补 directory-store 的 helper 和 4 条 focused RED。
2. 新增 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，只实现最小 backend-private store。
3. 跑 focused suite 收 GREEN，再跑 adjacent / completeness / compile / hygiene。
4. 用 fresh evidence 更新 roadmap / findings / progress / task_plan，并把 next queue 收紧到 directory-store 的更重 durability family。

## Task 1: RED - Lock the second local persistence shape

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Add directory-store helpers**
- 新增最小 helper：
  - `BuildReplayProviderStoreDirectoryPath(...)`
  - `CleanupReplayProviderStoreDirectory(...)`
  - `WriteDirectoryReplayStoreEntry(...)`
  - `WriteCorruptDirectoryReplayStoreEntry(...)`
- helper 只服务 focused contracts，不改变现有 file-backed helper family。

**Step 2: Add direct provider contracts**
- `TestDirectoryReplayStorePreservesReplayTruthAcrossProviderRebuild`
- `TestDirectoryReplayStorePrunesExpiredPersistedEntriesAfterRebuild`

**Step 3: Add runtime contracts**
- `TestDirectoryReplayStoreInstallHelperUsesReplayTruthAtRuntime`
- `TestDirectoryReplayStoreFailsClosedOnCorruptEntryAtRuntime`

**Step 4: Verify RED**
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - fresh RED because `fafafa.ssl.freepascal.earlydatareplay.dirstore` does not exist yet

## Task 2: GREEN - Add the smallest directory-backed store

**Files:**
- Create: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Implement the backend-private store**
- 新增：
  - `TFreePascalDirectoryEarlyDataReplayStore`
  - `TFreePascalDirectoryEarlyDataReplayStoreGuard`
- contract 固定为：
  - `AcquireUpdateGuard(...)`
  - `LoadEntries(...)`
  - `SaveEntries(...)`

**Step 2: Keep on-disk behavior minimal**
- `LoadEntries(...)`：
  - root 不存在 => empty store
  - root 不是目录 / child path 非法 / payload 非法 / trailing garbage => fail closed
  - 每个 entry 文件 payload 固定为 `Int32 version + TDateTime expiresAt`
- `SaveEntries(...)`：
  - 用 `<store>.tmpdir` 写完整快照
  - 若 canonical store 已存在，则先 rename 到 `<store>.bakdir`
  - 再把 `<store>.tmpdir` rename 到 canonical store
  - 成功后 best-effort 删除 `<store>.bakdir`
  - 失败时 best-effort 清理 `<store>.tmpdir`

**Step 3: Re-run focused suite**
- same command as Task 1
- Expected:
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`

## Task 3: Verify and close out

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run adjacent / full verification**
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_prototype_20260416`
- `python3 scripts/compile_all_modules.py`

**Step 2: Record truth**
- 更新 roadmap / working-memory，明确：
  - 第二 concrete store shape 已经落地
  - direct/runtime contract 已经锁住 rebuild / prune / fail-closed truth
  - default shipped path 仍然没有升级为 durable-by-default
  - next queue 转到 directory-store 的更重 durability / crash / cross-process family

**Step 3: Diff hygiene**
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-directory-store-prototype.md src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas tests/test_freepascal_tls13_early_data.pas docs/ROADMAP.md task_plan.md findings.md progress.md`
