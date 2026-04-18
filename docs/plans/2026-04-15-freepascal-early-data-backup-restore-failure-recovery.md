# FreePascal Early-Data Backup Restore Failure Recovery Implementation Plan

**Goal:** 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，收口 file-backed replay-store 在 backup-assisted replace 上的 restore-failure residual risk：当 `main -> .bak` 成功、second `temp -> main` 失败、且 `.bak -> main` restore 也失败时，既有 persisted replay truth 仍应可通过最小恢复路径继续被消费，而不是因为 canonical main 缺失而完全丢失。

**Architecture:** 继续优先走 tests-first。先在 `tests/test_freepascal_tls13_early_data.pas` 用一个 scripted file-backed store 子类把 “first `temp -> main` fail / `main -> .bak` success / second `temp -> main` fail / `.bak -> main` fail” 做成 deterministic RED，再把同一组语义提升到 runtime `Accept` path。只有 fresh RED 落到 provider 时，才最小修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`：优先让 readable resolution 在 `main` 缺失且 `.tmp` 不存在时，把 `.bak` 作为 restore-failure-only 的 bounded fallback truth source。public API、builder / factory / installer surface 与 capability wording 都不变。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalStoreBackedEarlyDataReplayProvider`, file-backed replay store, TLS 1.3 scripted runtime tests, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先补 direct provider 的 deterministic restore-failure RED。
2. 再把同一组 backup recovery 语义提升到 runtime `Accept` path。
3. 只在 fresh RED 落到 provider 时，最小修改 readable resolution / provider internals。
4. 跑 focused / adjacent / completeness / compile / hygiene。
5. 回填 roadmap / findings / progress，把 next queue 收紧到 permission/write-failure 与 `.bak` residue semantics。

## Task 1: RED - Lock backup restore-failure recovery

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Add a scripted file-backed store subclass**
- 在 focused test 文件里新增最小 subclass，继续复用真实 file-backed store 逻辑。
- 子类只 override file-op wrappers，不重写 `SaveEntries(...)`。
- 脚本化序列固定为：
  - first `temp -> main` => fail
  - `main -> .bak` => success
  - second `temp -> main` => fail
  - `.bak -> main` => fail

**Step 2: Add a direct provider contract**
- 先用正常 file-backed provider materialize 既有 session A 的 canonical main truth。
- 再用 scripted store-backed provider 对 fresh session B 触发上面的 deterministic restore failure。
- 断言：
  - B acquire fail closed
  - canonical main 缺失
  - `.tmp` cleaned
  - `.bak` 保留
  - scripted ledger 对 A 立即继续 reject
  - normal provider rebuild 后，A 继续 reject
  - normal provider rebuild 后，B first accept，随后 replay reject

**Step 3: Add a runtime contract**
- 先用正常 installer file-backed path materialize A。
- 再用 `InstallStoreBackedReplayLedger(...)` 把 scripted file-backed store 装到真实 server context，驱动 B 的 resumed early-data。
- 断言：
  - resumed handshake 继续成功
  - B early-data rejected
  - canonical main 缺失但 `.bak` 保留
  - runtime path 对 A 立即继续 reject
  - normal installer rebuild 后，A 继续 reject，B first accept，然后 replay reject

## Task 2: GREEN - Add the smallest restore-failure recovery path

**Files:**
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Extend bounded readable resolution**
- `ResolveReadableStoreFileName(...)` 继续优先：
  - `main`
  - orphan `.tmp`
- 只有当 `main` 缺失且 `.tmp` 不存在时，才允许把 `.bak` 作为 restore-failure-only 的 bounded fallback truth source。

**Step 2: Keep fail-closed semantics**
- `SaveEntries(...)` 的 commit 顺序与 fail-closed contract 继续保持不变。
- `.bak` 仍然不是常规主路径；只在 restore-failure 导致 canonical main 缺失时，才允许读取。
- stale `.bak` 无法清理 / 后续写入失败时仍 fail closed。

**Step 3: Re-run focused test until GREEN**
- Expected: `✅ FreePascal TLS 1.3 early-data checks passed`

## Task 3: Verify and close out

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run verification**
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH"; bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_backup_restore_failure_recovery_20260415`
- `python3 scripts/compile_all_modules.py`

**Step 2: Record evidence**
- 更新 roadmap / working-memory，明确：
  - backup restore failure branch 是否已收口
  - `.bak` 是否进入受限恢复读路径
  - 新的 next queue 是否移动到 permission/write-failure 与 `.bak` residue semantics
  - capability wording 保持不变

**Step 3: Diff hygiene**
- `git diff --check -- docs/plans/2026-04-15-freepascal-early-data-backup-restore-failure-recovery.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas task_plan.md findings.md progress.md`
