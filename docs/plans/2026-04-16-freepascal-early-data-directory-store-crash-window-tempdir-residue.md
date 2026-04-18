# FreePascal Early-Data Directory Store Crash-Window Tempdir Residue Implementation Plan

**Goal:** 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 directory-store 下一条最高 ROI 剩余语义补成可执行合同：当 canonical `main` 缺失而 live `.tmpdir` residue 承载 replay truth 时，pure replay reject 与 repeated process restart 都不能误消费这份 residue；只有后续 fresh acquire / fresh resumed accept 真正写回时，才允许重新 materialize canonical `main` 并清掉 `.tmpdir`。

**Architecture:** 继续 tests-first，并严格收缩在 focused early-data harness。优先复用现有 `TFreePascalDirectoryEarlyDataReplayStore`、`TFreePascalStoreBackedEarlyDataReplayProvider`、`InstallStoreBackedReplayLedger(...)`、`RunReplayProviderRuntimeReplayProbeMode(...)`、`MoveCanonicalReplayStoreDirectoryToFallback(...)` 与 child self-exec 机制，不新增 production seam。direct path 先锁“repeated replay reject preserves live `.tmpdir` residue”；runtime path 再锁“repeated restart replay-only probes preserve `.tmpdir` residue，直到 fresh resumed accept 才 consume fallback”。只有 fresh RED 明确指向 production drift 时，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data focused runtime tests, backend-private store-backed replay seam, directory-backed local persistence, child-process self-exec, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先补 directory-store `.tmpdir` residue direct/runtime 合同，明确 pure replay reject 不等于 consume fallback。
2. focused RED 先跑出来；如果只是 harness 缺口，就只补最小 tests-only helper。
3. 如有必要才最小改 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`；否则保持 tests-only closeout。
4. 跑 focused / adjacent / completeness / compile / diff hygiene，再回写 roadmap / working-memory，把 next queue 收紧到 fallback corruption / filesystem blocker。

## Task 1: RED - Lock Direct Tempdir Residue Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Add a repeated replay-reject direct contract**
- 复用现有：
  - `BuildReplayProviderStoreDirectoryPath(...)`
  - `CleanupReplayProviderStoreDirectory(...)`
  - `MoveCanonicalReplayStoreDirectoryToFallback(...)`
  - existing directory-store provider / ledger construction
- 新增 focused direct contract，覆盖：
  - canonical replay truth 先 materialize
  - 再切成 live `.tmpdir` residue
  - 第一次 replay reject 后，canonical `main` 继续缺失、`.tmpdir` 继续保留
  - 第二次 provider rebuild + replay reject 仍然成立
  - 只有后续 fresh acquire 才重新 materialize canonical `main` 并 consume `.tmpdir`

**Step 2: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - 如果 residue contract 还没被 harness / implementation 正确表达，focused suite 应给 fresh RED

## Task 2: RED - Lock Runtime Restart Residue Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Extend runtime replay probe with a replay-only expectation**
- 只补最小 tests-only helper 能力：
  - 为 `RunReplayProviderRuntimeReplayProbeMode(...)` 增加一个 replay-only expectation
  - replay-only child 只执行 replay reject，不在同一 child 内继续 capture/accept fresh session
- 不新增 production code，不新增新的 child mode。

**Step 2: Add a repeated restart contract**
- 新增 focused runtime contract，覆盖：
  - parent 先 materialize canonical directory replay truth
  - 切成 live `.tmpdir` residue
  - child #1 replay-only restart probe reject same session，并保持 canonical `main` 缺失、`.tmpdir` 保留
  - child #2 replay-only restart probe 再次 reject same session，语义不漂移
  - child #3 normal replay probe 继续 reject same session，随后 fresh resumed accept 才重新 materialize canonical `main` 并 consume `.tmpdir`

## Task 3: GREEN - Only If RED Proves A Real Production Gap

**Files:**
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Keep production scope minimal**
- 仅当 fresh RED 明确指向 directory-store drift：
  - 优先只改 `LoadEntries(...)` / `SaveEntries(...)` 周边
  - 不改 public API、builder / factory / config surface
  - 不改 `TFreePascalContext` / `TFreePascalConnection`

## Task 4: Verify And Record

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run focused and adjacent verification**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_crash_window_tempdir_residue_20260416`
  - `python3 scripts/compile_all_modules.py`

**Step 2: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-directory-store-crash-window-tempdir-residue.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas docs/ROADMAP.md task_plan.md findings.md progress.md`

## Definition Of Done

- directory-store `.tmpdir` residue 在 direct provider path 上被 repeated replay-reject contracts 锁住
- directory-store `.tmpdir` residue 在 repeated restart runtime path 上被 replay-only + recovery contracts 锁住
- 若 implementation 其实已天然满足，本批允许 tests-only closeout
- capability wording、public surface、`TFreePascalContext`、`TFreePascalConnection` 都保持不变
- focused tests、adjacent regressions、completeness gate、compile gate、diff hygiene 都有 fresh evidence
