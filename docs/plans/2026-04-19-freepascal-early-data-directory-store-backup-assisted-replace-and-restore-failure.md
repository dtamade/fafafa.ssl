# FreePascal Early-Data Directory-Store Backup-Assisted Replace And Restore-Failure Implementation Plan

**Goal:** 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 directory-backed replay-store 更深一层的 update-path write-interruption family 锁成 focused contracts：当 canonical `main` 已存在、`main -> .bakdir` 已成功后，若 `tempdir -> main` 失败，则 store-backed provider/runtime 必须保住旧 replay truth；若随后的 `.bakdir -> main` restore 也失败，则仍要 fail closed，并允许后续从 `.bakdir` fallback 恢复。

**Architecture:** 继续严格 tests-first，并完全复用现有 `TFreePascalDirectoryEarlyDataReplayStore`、`TFreePascalStoreBackedEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger`、`BuildReplayProviderStoreDirectoryPath(...)`、`CleanupReplayProviderStoreDirectory(...)`、`BuildDirectoryReplayStoreServerContext(...)`、`InstallStoreBackedReplayLedger(...)`、`AssertResumedEarlyDataAcceptedAtRuntime(...)` 与 `AssertResumedEarlyDataRejectedAtRuntime(...)`。先在 `tests/test_freepascal_tls13_early_data.pas` 镜像 file-backed `existing-main replace fallback failure` / `backup restore failure recovery` 家族补 direct/runtime 合同；只有 fresh RED 明确落到 directory-store rename 操作缺乏脚本化 seam，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，且只允许增加内部 `RenamePathAt(...)` override seam，不重开 `SaveEntries(...)` 算法、public wiring 或 capability wording。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data focused contracts, directory-backed local replay store, store-backed replay seam, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先补两个 scripted directory-store 子类，分别注入：
   - `tempdir -> main` 失败但 `.bakdir -> main` restore 成功
   - `tempdir -> main` 失败且 `.bakdir -> main` restore 也失败
2. 再补 direct provider 合同，锁住：
   - backup-assisted replace failure preserves existing truth
   - backup restore failure leaves canonical `main` missing but `.bakdir` truth remains recoverable
3. 再补 real runtime resumed early-data 合同，保证 store-backed runtime `Accept` path 也保持同一语义。
4. focused 跑 fresh RED；如果只是缺 rename seam，就做最小 `protected virtual RenamePathAt(...)` 暴露，不改 `SaveEntries(...)` 分支语义。
5. 跑 focused / adjacent / completeness / compile / diff hygiene，再回写 roadmap / working-memory。

## Task 1: RED - Lock Backup-Assisted Replace Failure Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Add a scripted replace-failure directory store**
- 新增最小 subclass：
  - `TScriptedExistingMainReplaceFailureDirectoryReplayStore`
- 约束：
  - 不重写 `SaveEntries(...)`
  - 只在 `RenamePathAt(<store>.tmpdir, <store>)` 第一次时 deterministic 返回 `False`

**Step 2: Add direct/runtime contracts**
- direct：
  - initial acquire 先 materialize canonical `main`
  - scripted blocked session fail closed
  - canonical `main` 继续存在，`.tmpdir` cleaned，`.bakdir` 不残留
  - original replay truth 立即继续 reject
  - rebuild 后 blocked session first accept，后续 replay reject
- runtime：
  - real resumed early-data accept path 先 materialize canonical `main`
  - store-backed scripted runtime blocked session early-data reject
  - canonical `main` preserved，`.tmpdir` cleaned，`.bakdir` 不残留
  - rebuild 后 blocked session 恢复 first accept，后续 replay reject

## Task 2: RED - Lock Backup Restore Failure Recovery

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Add a scripted restore-failure directory store**
- 新增最小 subclass：
  - `TScriptedBackupRestoreFailureDirectoryReplayStore`
- 约束：
  - `RenamePathAt(<store>.tmpdir, <store>)` 第一次 deterministic 返回 `False`
  - `RenamePathAt(<store>.bakdir, <store>)` deterministic 返回 `False`

**Step 2: Add direct/runtime contracts**
- direct：
  - blocked session fail closed
  - canonical `main` 缺失，`.tmpdir` cleaned，`.bakdir` 保留
  - original replay truth 继续可通过 `.bakdir` fallback reject
  - rebuild 后 blocked session first accept，恢复 canonical `main` 并消费 `.bakdir`
- runtime：
  - store-backed scripted runtime blocked session early-data reject
  - canonical `main` 缺失，`.tmpdir` cleaned，`.bakdir` 保留
  - original replay truth 继续 reject
  - runtime rebuild 后 blocked session 恢复 first accept，恢复 canonical `main` 并消费 `.bakdir`

## Task 3: GREEN - Keep The Fix Bounded

**Files:**
- Modify only if needed: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Add the smallest internal rename seam only if RED requires it**
- 只有 fresh RED 落到“directory-store rename 无法 script”时，才允许最小修法：
  - 新增 `RenamePathAt(const ASourcePath, const ADestPath: string): Boolean`
  - 默认仍调用 `RenameFile(...)`
  - 只替换 `SaveEntries(...)` 内部路径 rename 调用点
  - 不改 public API

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
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_backup_restore_families_20260419`
  - `python3 scripts/compile_all_modules.py`
  - `git diff --check -- docs/plans/2026-04-19-freepascal-early-data-directory-store-backup-assisted-replace-and-restore-failure.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas docs/ROADMAP.md task_plan.md findings.md progress.md`

### Definition Of Done

- directory-store backup-assisted replace failure semantics are covered by direct and runtime contracts
- directory-store backup restore failure recovery semantics are covered by direct and runtime contracts
- blocked session fail-closed does not silently lose existing replay truth
- rebuild/recovery from `.bakdir` continues to restore canonical `main` and replay rejection truth
- focused / adjacent / completeness / compile / diff hygiene all have fresh evidence
- capability wording remains `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
