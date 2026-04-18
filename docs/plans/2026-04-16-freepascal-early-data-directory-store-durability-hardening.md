# FreePascal Early-Data Directory Store Durability Hardening Implementation Plan

**Goal:** 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把刚落地的 `TFreePascalDirectoryEarlyDataReplayStore` 从 prototype 推进到第一批真正有价值的 durability hardening：锁住 cross-process lock fail-closed、orphan lock ignore、以及 `.tmpdir` / `.bakdir` replay truth 在 provider rebuild 与 runtime restart 两条路径上的 bounded recovery。

**Architecture:** 继续优先走 tests-first，并完全复用现有 `IFreePascalEarlyDataReplayStore`、`TFreePascalStoreBackedEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger` 与 `InstallStoreBackedReplayLedger(...)` 接线。tests/harness 侧只补 directory-store fallback / runtime child helper 所需的最小能力；生产改动继续严格收缩在 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas` 内部，把 readable directory resolution 明确为 `main > .tmpdir > .bakdir`，并保持 fail-closed：只要 canonical `main` 已存在但形态不对，就不允许静默回退到 fallback。整个批次不重开 file-backed `.bak` family，也不碰 public path wiring。

**Tech Stack:** FreePascal (ObjFPC), `IFreePascalEarlyDataReplayStore`, `TFreePascalStoreBackedEarlyDataReplayProvider`, `InstallStoreBackedReplayLedger(...)`, directory-backed local persistence, TLS 1.3 scripted runtime tests, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先在 `tests/test_freepascal_tls13_early_data.pas` 补 directory-store durability contracts，优先锁 direct provider 与 runtime restart 两条最值钱的 truth。
2. 再最小修改 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`，让 readable resolution 支持 `main > .tmpdir > .bakdir` 的 bounded recovery，并保持 bad-canonical fail-closed。
3. focused suite 过绿后，跑 backend basic、capability cache、completeness gate、compile-all。
4. 最后把 roadmap / task_plan / findings / progress 回写到“directory-store durability hardening 已收口，next queue 继续缩到 crash-window / fallback corruption / filesystem blocker”。

## Task 1: RED - Lock directory-store durability truth

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Extend helpers only where durability contracts need them**
- 补最小 tests-only helper：
  - directory-store cleanup 同时清 `.ready` / `.release` / `.session.bin` / `.graceful` / `.context_path`
  - `MoveCanonicalReplayStoreDirectoryToFallback(...)`
  - runtime replay child helper 的 `directory_store` branch
- 不新增新的 public helper，也不分叉新的 child mode。

**Step 2: Add direct provider durability contracts**
- `TestDirectoryReplayStoreFailsClosedWhileCrossProcessLockIsHeld`
- `TestDirectoryReplayStoreIgnoresOrphanLockFileAcrossProviderRebuild`
- `TestDirectoryReplayStoreRecoversReplayTruthFromOrphanTempDirectoryAcrossProviderRebuild`
- `TestDirectoryReplayStoreRecoversReplayTruthFromBackupDirectoryAcrossProviderRebuild`

**Step 3: Add runtime durability contracts**
- `TestDirectoryReplayStoreFailsClosedWhileCrossProcessLockIsHeldAtRuntime`
- `TestDirectoryReplayStoreRetainsReplayTruthAcrossProcessRestartFromOrphanTempDirectory`
- `TestDirectoryReplayStoreRetainsReplayTruthAcrossProcessRestartFromBackupDirectory`

**Step 4: Verify fresh RED**
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - fresh RED should point at missing directory-store fallback / restart truth, not at unrelated API drift

## Task 2: GREEN - Keep directory-store hardening bounded

**Files:**
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Add bounded readable-directory resolution**
- `ResolveReadableDirectoryName(...)` 明确：
  - `main` 存在且是目录 => 读 `main`
  - `main` 存在但不是目录 => fail closed
  - `main` 缺失时才依次看 `.tmpdir`、`.bakdir`
  - fallback 只接受目录；sidecar path 形态错误继续 fail closed

**Step 2: Reuse the same load validation path**
- 把读取拆成：
  - `ResolveReadableDirectoryName(...)`
  - `LoadEntriesFromDirectory(...)`
- `.tmpdir` / `.bakdir` fallback 继续走与 canonical `main` 相同的 entry validation / trailing-bytes reject 路径。

**Step 3: Keep write semantics unchanged except for recovery compatibility**
- 不改 public contract。
- `SaveEntries(...)` 继续写 `<store>.tmpdir`、rename old `main` 到 `.bakdir`、再 promote temp snapshot 到 canonical `main`。
- fallback 被消费后，后续正常 acquire/save 继续 re-materialize canonical `main` 并清掉用过的 `.tmpdir` / `.bakdir`。

**Step 4: Re-run focused suite**
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
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_durability_hardening_20260416`
- `python3 scripts/compile_all_modules.py`

**Step 2: Record truth**
- 更新 roadmap / working-memory，明确：
  - directory-store 现在已经不只是 prototype，而是补齐了第一批 durability hardening
  - cross-process lock fail-closed、orphan lock ignore、`.tmpdir` / `.bakdir` recovery 已有 direct/runtime evidence
  - capability wording 仍保持不变
  - next queue 缩到 crash-window / fallback corruption / filesystem blocker，而不是重开 file-backed 或 public wiring

**Step 3: Diff hygiene**
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-directory-store-durability-hardening.md src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas tests/test_freepascal_tls13_early_data.pas docs/ROADMAP.md task_plan.md findings.md progress.md`
