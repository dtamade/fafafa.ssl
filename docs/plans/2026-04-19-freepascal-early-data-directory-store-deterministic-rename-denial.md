# FreePascal Early-Data Directory-Store Deterministic Rename-Denial Closeout Implementation Plan

**Goal:** 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 directory-store update-path 上两条 deterministic rename-denial 子族锁成 focused contracts：`tempdir -> main` denied 必须 fail closed 并在后续 rebuild 后恢复；`main -> .bakdir` denied 必须 fail closed 且 preserve canonical `main` truth。

**Architecture:** 继续严格 tests-first，并完全复用现有 `TFreePascalDirectoryEarlyDataReplayStore`、`TFreePascalStoreBackedEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger`、`BuildReplayProviderStoreDirectoryPath(...)`、`CleanupReplayProviderStoreDirectory(...)`、`BuildDirectoryReplayStoreServerContext(...)`、`InstallStoreBackedReplayLedger(...)`、`AssertResumedEarlyDataAcceptedAtRuntime(...)` 与 `AssertResumedEarlyDataRejectedAtRuntime(...)`。这批直接消费上一批刚加好的 `RenamePathAt(...)` seam，在 `tests/test_freepascal_tls13_early_data.pas` 镜像 file-backed deterministic rename-denial 家族补 direct/runtime 合同；只有 fresh RED 明确落到 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas` 行为漂移时，才允许最小查看 `src/`，否则保持 tests/docs/working-memory only closeout。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data focused contracts, directory-backed local replay store, store-backed replay seam, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先补两个 scripted directory-store 子类，分别注入 deterministic `tempdir -> main` denied 与 deterministic `main -> .bakdir` denied。
2. 再补 direct provider 合同，锁住：
   - `tempdir -> main` denied 时 fail closed、canonical `main` 不误 materialize、后续 rebuild 恢复 accept
   - `main -> .bakdir` denied 时 fail closed、canonical `main` truth preserved、后续 rebuild 恢复 accept
3. 再补 real runtime resumed early-data 合同，保证 store-backed runtime `Accept` path 也保持同一语义。
4. 跑 focused suite；如果 current source 已经直接 GREEN，就保持 tests-only closeout，不补任何新的生产修法。
5. 跑 focused / adjacent / completeness / compile / diff hygiene，再回写 roadmap / working-memory。

## Task 1: RED - Lock Deterministic `tempdir -> main` Denied Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Add a scripted temp-promotion denied directory store**
- 新增最小 subclass：
  - `TScriptedTempPromotionRenameDeniedDirectoryReplayStore`
- 约束：
  - 不重写 `SaveEntries(...)`
  - 只在 `RenamePathAt(<store>.tmpdir, <store>)` 时 deterministic 返回 `False`

**Step 2: Add direct/runtime contracts**
- direct：
  - blocked session first acquire fail closed
  - canonical `main` absent，`.tmpdir` cleaned，`.bakdir` absent
  - rebuild 后 blocked session first accept，随后 replay reject
- runtime：
  - store-backed scripted runtime blocked session early-data reject
  - canonical `main` absent，`.tmpdir` cleaned，`.bakdir` absent
  - runtime rebuild 后 blocked session 恢复 first accept，随后 replay reject

## Task 2: RED - Lock Deterministic `main -> .bakdir` Denied Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Add a scripted backup-promotion denied directory store**
- 新增最小 subclass：
  - `TScriptedBackupPromotionRenameDeniedDirectoryReplayStore`
- 约束：
  - 不重写 `SaveEntries(...)`
  - 只在 `RenamePathAt(<store>, <store>.bakdir)` 时 deterministic 返回 `False`

**Step 2: Add direct/runtime contracts**
- direct：
  - initial acquire 先 materialize canonical `main`
  - blocked session fail closed
  - canonical `main` preserved，`.tmpdir` cleaned，`.bakdir` absent
  - original replay truth 立即继续 reject
  - rebuild 后 blocked session first accept，随后 replay reject
- runtime：
  - real resumed early-data accept path 先 materialize canonical `main`
  - store-backed scripted runtime blocked session early-data reject
  - canonical `main` preserved，`.tmpdir` cleaned，`.bakdir` absent
  - original replay truth 立即继续 reject
  - runtime rebuild 后 blocked session 恢复 first accept，随后 replay reject

## Task 3: GREEN - Keep The Fix Bounded

**Files:**
- Reference only unless fresh RED requires it: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Only touch `src/` if focused RED proves real drift**
- 若 focused suite 直接 GREEN：
  - 保持 tests-only closeout
  - 不改 `SaveEntries(...)`
  - 不改 `RenamePathAt(...)`
  - 不改 public API / builder / factory / runtime wiring
- 只有 fresh RED 明确证明行为漂移时，才允许最小修 `dirstore`

## Task 4: Verify And Close Out

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run adjacent verification**
- Run:
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_deterministic_rename_denial_20260419`
  - `python3 scripts/compile_all_modules.py`
  - `git diff --check -- docs/plans/2026-04-19-freepascal-early-data-directory-store-deterministic-rename-denial.md tests/test_freepascal_tls13_early_data.pas docs/ROADMAP.md task_plan.md findings.md progress.md`

### Definition Of Done

- directory-store deterministic `tempdir -> main` denied semantics are covered by direct and runtime contracts
- directory-store deterministic `main -> .bakdir` denied semantics are covered by direct and runtime contracts
- focused fresh evidence can close this batch without new production edits if current source already satisfies the contracts
- focused / adjacent / completeness / compile / diff hygiene all have fresh evidence
- capability wording remains `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
