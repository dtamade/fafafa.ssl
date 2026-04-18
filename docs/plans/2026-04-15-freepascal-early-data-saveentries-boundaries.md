# FreePascal Early-Data SaveEntries Boundaries Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 file-backed replay-store 的 `SaveEntries(...)` 再补一批高 ROI 的持久化写入边界合同：`.tmp` 写入失败时保住既有 replay truth，canonical main-path replace / rename 边界失败时 fail closed，blocker 移除后 recovery 重新 accept，随后 replay 继续 reject。

**Architecture:** 继续优先落在 `tests/test_freepascal_tls13_early_data.pas`。direct provider 与 installer runtime 合同都复用现有 file-backed helper、runtime assertion helper 与 blocker cleanup path；只有 fresh RED 明确落到 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 的 `SaveEntries(...)` 漂移时，才做最小 internal 修复。默认目标仍是 tests/docs/working-memory only。

**Tech Stack:** FreePascal (ObjFPC), file-backed early-data replay provider, focused TLS 1.3 early-data runtime tests, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先补 direct provider 的 `SaveEntries(...)` failure/recovery 合同。
2. 再把同一组 failure/recovery 语义提升到 installer runtime resumed early-data `Accept` path。
3. 先跑 focused test 观察 fresh RED；只有 fresh RED 明确指出 provider drift 时才最小看 `src/`。
4. 跑 adjacent regressions、completeness gate、compile gate。
5. 回填 roadmap / working-memory，把 next queue 收紧到 permission/write-failure 或更重的 interrupted-write 边界。

## Task 1: RED - Lock direct provider SaveEntries boundaries

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Add temp-path write-failure preserves-old-truth contract**
- 先 materialize canonical main store 上的 live replay truth A。
- 再把 `AFileName + '.tmp'` 做成 directory blocker，制造 temp create/write failure。
- 断言：
  - fresh session B acquire fail closed
  - canonical main store file 仍存在，且 bytes 保持失败前内容
  - replay truth A 仍 reject
  - blocker 移除后，B 重新 accept
  - provider rebuild 后，B replay reject

**Step 2: Add canonical main-path rename-boundary fail-closed contract**
- 让 canonical `AFileName` 自身被 directory blocker 占位，制造 temp-to-main rename/replace 边界失败。
- 断言：
  - fresh session acquire fail closed
  - canonical main store file 不会 materialize 成普通文件
  - blocker 移除后，同一 session 重新 accept
  - provider rebuild 后 replay reject

**Step 3: Run focused test to observe RED**
- Run: `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected: FAIL on one of the new direct provider contracts.

## Task 2: RED - Lift SaveEntries boundaries to runtime

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add runtime temp-path write-failure preserves-old-truth contract**
- 先通过 installer runtime path materialize canonical replay truth A。
- 再对同一个 replay-store file 制造 `.tmp` directory blocker。
- 断言：
  - fresh session B resumed early-data reject，但 handshake 继续成功
  - canonical main store file 仍保留旧 truth
  - runtime 上 A 继续 reject
  - blocker 移除后 B 重新 accept
  - 随后 B replay reject

**Step 2: Add runtime canonical main-path rename-boundary fail-closed contract**
- 让 installer runtime replay-store file path 自身被 directory blocker 占位。
- 断言：
  - fresh resumed early-data fail closed
  - canonical main replay store file 不 materialize
  - blocker 移除后同一 session 重新 accept
  - 下一次 replay reject

**Step 3: Re-run focused test**
- Expected:
  - 先看到 fresh RED
  - 如果 current implementation 已满足合同，focused suite 直接 GREEN

## Task 3: GREEN - Make the smallest fix only if RED proves drift

**Files:**
- Default: no `src/` changes
- Fallback Modify: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Keep the fix bounded to SaveEntries / cleanup semantics**
- 只允许查看并最小修改：
  - `TFreePascalFileEarlyDataReplayStore.SaveEntries(...)`
  - 与 `.tmp` cleanup / replace failure directly coupled 的 private behavior
- 不改 public API / builder / factory / context / connection wiring。

**Step 2: Re-run focused test until GREEN**
- Expected: `✅ FreePascal TLS 1.3 early-data checks passed`

## Task 4: Verify and close out

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run adjacent verification**
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_saveentries_boundaries_20260415`
- `python3 scripts/compile_all_modules.py`

**Step 2: Record evidence**
- 更新 roadmap / working-memory，明确：
  - `SaveEntries(...)` temp/main-path 边界是否已天然满足合同
  - 本批是否保持 tests-only
  - next queue 是否进一步收紧到 permission/write-failure 或更重 interrupted-write shape

**Step 3: Diff hygiene**
- `git diff --check -- docs/plans/2026-04-15-freepascal-early-data-saveentries-boundaries.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
