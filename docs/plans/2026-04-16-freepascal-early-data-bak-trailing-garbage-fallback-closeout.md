# FreePascal Early-Data `.bak` Trailing-Garbage Fallback Closeout Implementation Plan

**Goal:** 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 file-backed replay-store 的 `.bak` fallback 读路径补齐最后一个极小 corruption sidecar：当 `main` 缺失、`.tmp` 不存在、provider 退回读取 `.bak`，即使 `.bak` 拥有合法 header + 合法 entry，只要尾部还带 trailing garbage bytes，也必须继续 fail closed，且不允许 silent accept、silent heal、或隐式重建 canonical main。

**Architecture:** 继续优先走 tests-first，并完全复用现有 `.bak` fallback corruption harness。只在 `tests/test_freepascal_tls13_early_data.pas` 里新增一个 trailing-garbage fixture helper，并把 direct provider / installer runtime 两条现有 corruption 矩阵各加一个 `trailing_garbage` case。默认不动 `src/`；只有 fresh RED 明确指出 `.bak` fallback 没有复用 `LoadEntries(...)` 的 trailing-bytes fail-closed 语义时，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalStoreBackedEarlyDataReplayProvider`, file-backed replay store, TLS 1.3 scripted runtime tests, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先补 2026-04-16 trailing-garbage closeout 计划与 working-memory 入口。
2. 在 `tests/test_freepascal_tls13_early_data.pas` 里新增 `.bak` trailing-garbage fixture helper。
3. 在 direct provider 与 installer runtime 两条现有 `.bak` corruption 矩阵里各追加一个 `trailing_garbage` case。
4. 跑 focused / adjacent / completeness / compile / hygiene，并用 fresh evidence 回填 roadmap / findings / progress / task_plan。

## Task 1: RED - Lock `.bak` trailing-garbage fail-closed semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Add a trailing-garbage fixture helper**
- 在现有 binary fixture helpers 旁边新增一个最小 helper：
  - 先写入合法 version / count / key / expiresAt 的单 entry store
  - 再在文件末尾追加固定垃圾字节
- helper 只负责构造 fixture，不引入新的 provider/store subclass。

**Step 2: Extend direct provider coverage**
- 复用 `TestFileBackedReplayProviderFailsClosedOnCorruptBackupFallbackStores` 的现有 matrix。
- 新增一个 `trailing_garbage` case，固定断言：
  - fresh blocked session 继续 reject
  - original replay truth 不会被误恢复
  - canonical main 继续缺失
  - `.tmp` 不残留
  - corrupt `.bak` 保留

**Step 3: Extend runtime coverage**
- 复用 `TestContextFileBackedReplayInstallerFailsClosedOnCorruptBackupFallbackStoresAtRuntime` 的现有 matrix。
- 新增一个 `trailing_garbage` case，固定断言：
  - resumed handshake 继续成功
  - session 继续 reused
  - early-data 被 reject
  - discarded early bytes 不可读
  - canonical main 不会被隐式重建
  - corrupt `.bak` 仍保留

## Task 2: GREEN - Keep production scope minimal

**Files:**
- Default: no `src/` changes
- Fallback Modify: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Re-run focused suite**
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - 若当前 provider 已天然用 `LStream.Position <> LStream.Size` reject trailing bytes，则 suite 直接 GREEN
  - 若 fresh RED 明确指出 `.bak` fallback 漏掉这套校验，才允许最小修补 load path

**Step 2: Bound any fallback fix**
- 只允许最小查看并修改：
  - `ResolveReadableStoreFileName(...)`
  - `LoadEntries(...)`
- 不改 `SaveEntries(...)`、installer/public/runtime wiring、builder / factory / config surface。

## Task 3: Verify and close out

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run verification**
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_bak_trailing_garbage_20260416`
- `python3 scripts/compile_all_modules.py`

**Step 2: Record evidence**
- 更新 roadmap / working-memory，明确：
  - `.bak` trailing-garbage fallback 是否继续 fail closed
  - runtime resumed early-data path 是否与 direct provider truth 对齐
  - 本批是否保持 tests-only
  - next queue 是否回到更重的 provider / durability / persistence family

**Step 3: Diff hygiene**
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-bak-trailing-garbage-fallback-closeout.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas task_plan.md findings.md progress.md`
