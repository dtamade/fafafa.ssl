# FreePascal Early-Data Directory-Store Blocker Edge Closeout Implementation Plan

**Goal:** 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` 或 `TFreePascalConnection` wiring 的前提下，对 directory-backed replay-store 的 blocker family 做最后一轮最小边界扫尾：把 `.bakdir` wrong-shape blocker 在 first acquire 上的 direct/runtime fail-closed + recovery 语义补成 focused contracts，确认 blocker queue 真正收口。

**Architecture:** 继续严格 tests-first，并完全复用现有 `TFreePascalDirectoryEarlyDataReplayStore`、`TFreePascalStoreBackedEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger`、`BuildDirectoryReplayStoreServerContext(...)`、`AssertResumedEarlyDataAcceptedAtRuntime(...)`、`AssertResumedEarlyDataRejectedAtRuntime(...)`、`BuildReplayProviderStoreDirectoryPath(...)`、`CleanupReplayProviderStoreDirectory(...)` 与 `TouchFile(...)`。先只扩 `tests/test_freepascal_tls13_early_data.pas` 现有 blocker contracts；只有 fresh RED 明确落到 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，才最小修法。

**Tech Stack:** FreePascal (ObjFPC), directory-backed local replay store, TLS 1.3 early-data focused runtime tests, backend-private store-backed replay seam, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 在 direct provider blocker contract 里补 `.bakdir` regular-file first-acquire fail-closed/recovery 子用例。
2. 在 real runtime blocker contract 里补对应的 `.bakdir` first-acquire fail-closed/recovery 子用例。
3. 跑 focused suite，看这是不是纯 tests-only closeout，还是会打出 fresh production RED。
4. 仅在需要时最小修 `dirstore`；随后跑 adjacent / gate / compile / diff hygiene，并回写 roadmap / working-memory。

## Task 1: RED - Lock Direct `.bakdir` First-Acquire Blocker Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Steps:**
- 在现有 `TestDirectoryReplayStoreFailsClosedOnFilesystemPathBlockersAndRecovers` 里补一段最小 direct 子用例，覆盖：
  - canonical `main` 缺失
  - `.bakdir` 被 regular file 占住
  - first acquire fail closed
  - canonical `main` / `.tmpdir` 不 materialize
  - blocker file 保留
  - blocker 移除后，同一 session 恢复 accept
  - provider rebuild 后 replay truth 继续 reject

## Task 2: RED - Lock Runtime `.bakdir` First-Acquire Blocker Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Steps:**
- 在现有 `TestDirectoryReplayStoreFailsClosedOnFilesystemPathBlockersAtRuntime` 里补一段最小 runtime 子用例，覆盖：
  - canonical `main` 缺失
  - `.bakdir` 被 regular file 占住
  - resumed early-data `Accept` path fail closed
  - blocker file 保留，canonical `main` / `.tmpdir` 不 materialize
  - blocker 移除后，同一 resumed session 恢复 accept
  - rebuild 后 replay truth 继续 reject

## Task 3: GREEN - Keep Production Touches Conditional

**Files:**
- Modify only if needed: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Steps:**
- focused suite 若直接 GREEN，则保持 tests-only closeout。
- 若 fresh RED 指向 `SaveEntries(...)` 对 `.bakdir` wrong-shape first-acquire path 的处理，再做最小 fail-closed 修法，不重开 public wiring 或其他 provider family。

## Task 4: Verify And Close Out

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Verification:**
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_blocker_edge_closeout_20260416`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-directory-store-blocker-edge-closeout.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas docs/ROADMAP.md task_plan.md findings.md progress.md`

### Definition Of Done

- directory-store `.bakdir` regular-file first-acquire blocker is covered in both direct and runtime focused contracts
- fresh blocked session fail closed does not silently delete the blocker artifact
- blocker removal re-opens the blocked session and replay truth remains durable after rebuild
- focused / adjacent / completeness / compile / diff hygiene all have fresh evidence
- capability wording remains `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
