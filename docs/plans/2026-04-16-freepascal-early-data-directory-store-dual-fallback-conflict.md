# FreePascal Early-Data Directory-Store Dual Fallback Conflict Implementation Plan

**Goal:** 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` 或 `TFreePascalConnection` wiring 的前提下，把 directory-backed replay-store 更像真实 crash-window 的双 fallback 冲突态锁成 focused contracts：当 canonical `main` 缺失、优先级更高的 `.tmpdir` 已损坏、而次级 `.bakdir` 仍承载健康旧 replay truth 时，provider/runtime 都必须 fail closed，不能 silent heal 或直接跳过坏 `.tmpdir` 回退到 `.bakdir`；只有坏 `.tmpdir` 被显式移除后，才允许从健康 `.bakdir` 恢复。

**Architecture:** 继续严格 tests-first，并完全复用现有 `TFreePascalDirectoryEarlyDataReplayStore`、`TFreePascalStoreBackedEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger`、`BuildDirectoryReplayStoreServerContext(...)`、`AssertResumedEarlyDataAcceptedAtRuntime(...)`、`AssertResumedEarlyDataRejectedAtRuntime(...)`、`MoveCanonicalReplayStoreDirectoryToFallback(...)`、`WriteCorruptDirectoryReplayStoreEntry(...)` 与 path cleanup helpers。先只扩 `tests/test_freepascal_tls13_early_data.pas`；只有 fresh RED 明确落到 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，才最小修法。

**Tech Stack:** FreePascal (ObjFPC), directory-backed local replay store, TLS 1.3 early-data focused runtime tests, backend-private store-backed replay seam, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. direct provider path 先补 `corrupt .tmpdir + healthy .bakdir` 双 fallback 冲突合同。
2. real runtime path 再补同构 resumed early-data `Accept` 合同。
3. focused suite 观察这是 tests-only closeout 还是 fresh production RED。
4. 只有需要时最小修 `dirstore`；随后跑 adjacent / gate / compile / diff hygiene，并回写 roadmap / working-memory。

## Task 1: RED - Lock Direct Dual Fallback Conflict Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Steps:**
- 新增一条 direct provider 合同，覆盖：
  - canonical `main` 先 materialize 旧 replay truth
  - 再切到健康 `.bakdir`
  - 同时造出损坏 `.tmpdir`
  - fresh blocked session 必须 fail closed，不能跳过坏 `.tmpdir` 回退到 `.bakdir`
  - original replay truth 也继续 reject
  - canonical `main` 保持缺失，`.tmpdir` / `.bakdir` 都继续保留
  - 显式移除坏 `.tmpdir` 后，fresh blocked session 才恢复 accept，并重新 materialize canonical `main`

## Task 2: RED - Lock Runtime Dual Fallback Conflict Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Steps:**
- 新增一条 real runtime 合同，覆盖：
  - resumed early-data `Accept` path 先 materialize canonical `main`
  - 切到健康 `.bakdir`，同时造出损坏 `.tmpdir`
  - 新 runtime context 必须 fail closed，不允许 silent fallback 到 `.bakdir`
  - 显式移除坏 `.tmpdir` 后，fresh resumed early-data 才恢复 accept，并在 rebuild 后保持 replay reject

## Task 3: GREEN - Keep Production Touches Conditional

**Files:**
- Modify only if needed: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Steps:**
- focused suite 若直接 GREEN，则保持 tests-only closeout。
- 若 fresh RED 指向 dual-fallback resolution / load path / save path，再做最小 fail-closed 修法，不重开 public wiring 或其他 provider family。

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
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_dual_fallback_conflict_20260416`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-directory-store-blocker-edge-closeout.md docs/plans/2026-04-16-freepascal-early-data-directory-store-dual-fallback-conflict.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas docs/ROADMAP.md task_plan.md findings.md progress.md`

### Definition Of Done

- `corrupt .tmpdir + healthy .bakdir` 双 fallback 冲突态在 direct/runtime focused contracts 中被锁住
- preferred `.tmpdir` 损坏时不会 silent fallback 到 `.bakdir`
- 坏 `.tmpdir` 显式移除后，健康 `.bakdir` replay truth 才允许恢复并重新 materialize canonical `main`
- focused / adjacent / completeness / compile / diff hygiene all have fresh evidence
- capability wording remains `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
