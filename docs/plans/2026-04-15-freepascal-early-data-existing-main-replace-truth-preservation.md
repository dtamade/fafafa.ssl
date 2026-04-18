# FreePascal Early-Data Existing-Main Replace Truth Preservation Implementation Plan

**Goal:** 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，收口 file-backed replay-store `SaveEntries(...)` 在 existing-main replace fallback 上的 truth-preservation gap：当 first `temp -> main` 失败、fallback 迁移 old main 时，既有 persisted replay truth 不应因 second `temp -> main` 再失败而丢失。

**Architecture:** 继续优先走 tests-first。先在 `tests/test_freepascal_tls13_early_data.pas` 用一个 scripted file-backed store 子类把“first temp->main fail / main->bak success / second temp->main fail / bak->main restore success”做成 deterministic RED，再在 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 内部为 `TFreePascalFileEarlyDataReplayStore` 加最小 protected virtual file-op wrappers，并把 `SaveEntries(...)` 改成 backup-assisted replace。runtime path 继续复用已有 `InstallStoreBackedReplayLedger(...)` / installer helper，不新增 public API。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalStoreBackedEarlyDataReplayProvider`, file-backed replay store, TLS 1.3 scripted runtime tests, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先补 direct provider 的 deterministic existing-main replace fallback RED。
2. 再把同一组 truth-preservation 语义提升到 runtime `Accept` path。
3. 只在 fresh RED 落到 provider 时，最小给 file store 增加 overrideable file-op wrappers 与 backup-assisted replace。
4. 跑 focused / adjacent / completeness / compile / hygiene。
5. 回填 roadmap / findings / progress，把 next queue 收紧到 restore-failure / permission-write-failure。

## Task 1: RED - Lock existing-main replace truth preservation

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Add a scripted file-backed store subclass**
- 在 focused test 文件里新增一个最小 subclass，继续复用真实 file-backed store 逻辑。
- 子类只 override file-op wrappers，不重写 `SaveEntries(...)`。
- 脚本化序列固定为：
  - first `temp -> main` => fail
  - `main -> bak` => success
  - second `temp -> main` => fail
  - `bak -> main` => success

**Step 2: Add a direct provider contract**
- 先用正常 file-backed provider materialize 既有 session A 的 canonical main truth。
- 再用 scripted store-backed provider 对 fresh session B 触发上面的 deterministic replace failure。
- 断言：
  - B acquire fail closed
  - canonical main bytes 保持失败前内容
  - `.tmp` cleaned
  - `.bak` 不残留
  - normal provider rebuild 后，A 继续 reject
  - B 仍可在正常 provider 上 first accept，随后 replay reject

**Step 3: Add a runtime contract**
- 先用正常 installer file-backed path materialize A。
- 再用 `InstallStoreBackedReplayLedger(...)` 把 scripted file-backed store 装到真实 server context，驱动 B 的 resumed early-data。
- 断言：
  - resumed handshake 继续成功
  - B early-data rejected
  - canonical main bytes 未退化
  - normal installer rebuild 后，A 继续 reject，B first accept，然后 replay reject

**Step 4: Run focused test to observe RED**
- Run: `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected: FAIL on one of the new truth-preservation contracts.

## Task 2: GREEN - Minimal backup-assisted replace in provider

**Files:**
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Add minimal overrideable file-op wrappers**
- 只为 `TFreePascalFileEarlyDataReplayStore` 增加 protected virtual wrappers，至少覆盖：
  - file exists
  - delete file
  - rename file
  - create input/output stream
- 不改 constructor / helper 函数 / install helper public signature。

**Step 2: Convert SaveEntries to backup-assisted replace**
- 新增 canonical backup path：`<store>.bak`
- commit 顺序固定：
  - 写 temp
  - 尝试 `temp -> main`
  - 若失败且 main 存在：`main -> bak`
  - 再尝试 `temp -> main`
  - 若第二次仍失败：`bak -> main` 恢复旧 truth，并返回 `False`
  - 若新 main 成功：best-effort 清理 `.bak`
- `LoadEntries(...)` / readable resolution 不读取 `.bak`

**Step 3: Keep fail-closed semantics**
- stale `.bak` 无法清理 => fail closed
- restore `bak -> main` 失败 => 继续 fail closed
- `.tmp` cleanup 继续保留 outer `finally`

**Step 4: Re-run focused test until GREEN**
- Expected: `✅ FreePascal TLS 1.3 early-data checks passed`

## Task 3: Verify and close out

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run adjacent verification**
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH"; bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_existing_main_replace_truth_preservation_20260415`
- `python3 scripts/compile_all_modules.py`

**Step 2: Record evidence**
- 更新 roadmap / working-memory，明确：
  - existing-main replace fallback gap 是否已收口
  - 新的 next queue 是否移动到 restore-failure / permission-write-failure
  - capability wording 保持不变

**Step 3: Diff hygiene**
- `git diff --check -- docs/plans/2026-04-15-freepascal-early-data-existing-main-replace-truth-preservation.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas task_plan.md findings.md progress.md`
