# FreePascal Early-Data `.bak` Residue Semantics Implementation Plan

**Goal:** 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，收口 file-backed replay-store 的 `.bak` residue semantics：成功路径上 `.bak` cleanup 失败时，新 truth 仍应保留成功；而残留 `.bak` 在后续写入前无法删除时，provider / runtime path 仍应 fail closed，不丢旧 truth。

**Architecture:** 继续优先走 tests-first。先在 `tests/test_freepascal_tls13_early_data.pas` 新增一个最小 scripted file-backed store 子类，复用现有 `RenameFileAt(...)` failure 注入来稳定走 backup-assisted replace 分支，再通过 `DeleteFileAt(...)` 只对 `.bak` 删除失败做 deterministic 注入。direct provider 与 runtime `Accept` path 各补一条合同，把“cleanup failure leaves residue but still succeeds”与“stale residue undeletable on next save fails closed”锁进同一批。只有 fresh RED 明确落到 provider drift，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`；默认不重开 public API、builder / factory / installer seam 与 capability wording。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalStoreBackedEarlyDataReplayProvider`, file-backed replay store, TLS 1.3 scripted runtime tests, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先补一个 `.bak` delete-failure scripted store。
2. 再补 direct provider 的 `.bak` residue semantics focused contract。
3. 再把同一组语义提升到 runtime resumed early-data `Accept` path。
4. 跑 focused / adjacent / completeness / compile / hygiene。
5. 回填 roadmap / findings / progress，把 next queue 收紧到更真实的 permission/write-failure 形态。

## Task 1: RED - Lock `.bak` residue semantics with deterministic delete failure

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Add a scripted file-backed store subclass**
- 在现有 scripted replace/restore failure store 旁边新增一个最小 subclass。
- 子类继续只 override file-op wrappers，不重写 `SaveEntries(...)`。
- 脚本化语义固定为：
  - first `temp -> main` => fail
  - `main -> .bak` => success
  - second `temp -> main` => success
  - every `DeleteFileAt(<store>.bak)` => fail

**Step 2: Add a direct provider contract**
- 先用正常 file-backed provider materialize 既有 session A 的 canonical main truth。
- 再用 scripted store-backed provider 对 session B 触发成功路径 cleanup failure，断言：
  - B acquire 成功
  - canonical main 存在
  - `.tmp` cleaned
  - `.bak` residue 保留
  - A / B 都立即继续 reject，证明 main truth 没被 residue 覆盖
- 随后对 fresh session C 再触发一次写入，断言：
  - C acquire fail closed
  - canonical main bytes 保持 B 成功后的内容
  - `.tmp` 不残留
  - `.bak` residue 仍保留
- 最后用正常 provider rebuild，断言：
  - A / B 继续 reject
  - C first accept
  - recovery 后 `.bak` 被正常消费/清理
  - C replay reject

**Step 3: Add a runtime contract**
- 先用正常 installer file-backed path materialize A。
- 再用 `InstallStoreBackedReplayLedger(...)` 把 scripted file-backed store 装到真实 server context。
- 先驱动 B 的 resumed early-data，断言：
  - resumed handshake 成功
  - B early-data accepted
  - canonical main 存在
  - `.tmp` cleaned
  - `.bak` residue 保留
  - A / B 立即继续 reject
- 再驱动 C，断言：
  - resumed handshake 继续成功
  - C early-data rejected
  - canonical main bytes 保持 B 成功后的内容
  - `.tmp` 不残留
  - `.bak` residue 仍保留
- 最后用正常 installer rebuild，断言：
  - A / B 继续 reject
  - C first accept
  - recovery 后 `.bak` 被清理
  - C replay reject

## Task 2: GREEN - Keep provider changes strictly optional

**Files:**
- Default: no `src/` changes
- Fallback Modify: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Re-run focused test and inspect fresh RED**
- Expected:
  - 若当前 provider 已天然满足 `.bak` residue semantics，新 focused suite 直接 GREEN
  - 若 fresh RED 明确指出 provider drift，只允许最小修改 `SaveEntries(...)` / bounded `.bak` handling

**Step 2: Keep any fallback fix bounded**
- 只允许最小查看并修改：
  - `TFreePascalFileEarlyDataReplayStore.SaveEntries(...)`
  - 与 `.bak` pre-delete / post-success cleanup directly coupled 的 provider internals
- 不改 public API / builder / factory / context / connection wiring。

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
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH"; bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_bak_residue_semantics_20260415`
- `python3 scripts/compile_all_modules.py`

**Step 2: Record evidence**
- 更新 roadmap / working-memory，明确：
  - success-path `.bak` cleanup failure 是否继续 best-effort success
  - stale `.bak` undeletable on next save 是否继续 fail closed
  - 本批是否保持 tests-only，或最小 provider fix 落在哪
  - next queue 是否收紧到更真实的 permission/write-failure 形态
  - capability wording 保持不变

**Step 3: Diff hygiene**
- `git diff --check -- docs/plans/2026-04-15-freepascal-early-data-bak-residue-semantics.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas task_plan.md findings.md progress.md`
