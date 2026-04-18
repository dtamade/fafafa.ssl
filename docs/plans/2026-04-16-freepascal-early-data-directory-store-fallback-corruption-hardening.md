# FreePascal Early-Data Directory-Store Fallback Corruption Hardening Implementation Plan

**Goal:** 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 directory-backed replay-store 补齐 `.tmpdir` / `.bakdir` fallback corruption fail-closed 合同：当 canonical `main` 缺失而 provider/runtime 退回读取 fallback 目录时，`invalid_version` 与 `trailing_garbage` 都必须继续 fail closed，不允许 silent accept、silent heal、或隐式重建 canonical `main`。

**Architecture:** 继续严格 tests-first，并完全复用现有 `TFreePascalDirectoryEarlyDataReplayStore`、`TFreePascalStoreBackedEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger`、`BuildDirectoryReplayStoreServerContext(...)`、`AssertResumedEarlyDataAcceptedAtRuntime(...)`、`AssertResumedEarlyDataRejectedAtRuntime(...)`、`MoveCanonicalReplayStoreDirectoryToFallback(...)` 与 `WriteCorruptDirectoryReplayStoreEntry(...)`。先在 `tests/test_freepascal_tls13_early_data.pas` 镜像 file-backed `.bak` fallback corruption 模式补 direct/runtime 合同；只有 fresh RED 明确指向生产漂移时，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`。

**Tech Stack:** FreePascal (ObjFPC), directory-backed local replay store, TLS 1.3 early-data focused runtime tests, backend-private store-backed replay seam, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先补 directory-store fallback corruption direct/runtime 合同，覆盖 `.tmpdir` / `.bakdir` 与 `invalid_version` / `trailing_garbage`。
2. focused 跑 fresh evidence；如果当前实现已天然 fail-closed，就保持 tests-only closeout。
3. 只有 fresh RED 落到 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas` 时，才做最小生产修复。
4. 跑 focused / adjacent / completeness / compile / diff hygiene，再回写 roadmap / working-memory。

## Task 1: RED - Lock Direct Fallback Corruption Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Add a direct provider fallback-corruption contract**
- 复用现有：
  - `BuildReplayProviderStoreDirectoryPath(...)`
  - `CleanupReplayProviderStoreDirectory(...)`
  - `MoveCanonicalReplayStoreDirectoryToFallback(...)`
  - `WriteCorruptDirectoryReplayStoreEntry(...)`
- 覆盖：
  - canonical `main` 先 materialize
  - 再切成 `.tmpdir` / `.bakdir` fallback
  - fallback entry 改成 `invalid_version` / `trailing_garbage`
  - fresh blocked session fail closed
  - original replay truth 不被误恢复
  - canonical `main` 继续缺失，corrupt fallback artifact 继续保留

**Step 2: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`

## Task 2: RED - Lock Runtime Fallback Corruption Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Add a real runtime fallback-corruption contract**
- 复用现有：
  - `BuildDirectoryReplayStoreServerContext(...)`
  - `AssertResumedEarlyDataAcceptedAtRuntime(...)`
  - `AssertResumedEarlyDataRejectedAtRuntime(...)`
  - `MoveCanonicalReplayStoreDirectoryToFallback(...)`
  - `WriteCorruptDirectoryReplayStoreEntry(...)`
- 覆盖：
  - runtime accept path 先 materialize canonical `main`
  - `.tmpdir` / `.bakdir` fallback corruption 后，new runtime context 继续 fail closed
  - original session 与 fresh blocked session 都不能被 silent accept
  - canonical `main` 继续缺失，corrupt fallback artifact 继续保留

## Task 3: GREEN - Only If Fresh RED Demands It

**Files:**
- Modify only if needed: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Keep the fix bounded**
- 只有 fresh RED 明确落到：
  - fallback readable resolution
  - directory entry validation
  - corrupt fallback load path
- 才允许最小修复 `dirstore`；不重开 public wiring 或 file-backed family。

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
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_fallback_corruption_20260416`
  - `python3 scripts/compile_all_modules.py`
  - `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-directory-store-fallback-corruption-hardening.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas docs/ROADMAP.md task_plan.md findings.md progress.md`

### Definition Of Done

- corrupt `.tmpdir` / `.bakdir` fallback directories are covered by direct and runtime contracts
- `invalid_version` and `trailing_garbage` fallback entries both fail closed
- canonical `main` is not silently rebuilt from corrupt fallback state
- corrupt fallback artifact remains visible until explicit cleanup
- focused / adjacent / completeness / compile / diff hygiene all have fresh evidence
- capability wording remains `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
