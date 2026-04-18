# FreePascal Early-Data `.bak` Fallback Corruption Hardening Implementation Plan

**Goal:** 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 file-backed replay-store 新增 `.bak` fallback 读路径补齐腐坏 fail-closed 合同：当 `main` 缺失、`.tmp` 不存在、provider 退回读取 `.bak` 时，invalid version / truncated payload / invalid count / invalid key-length 都必须继续 fail closed，且不允许 silent accept、silent heal、或隐式重建 canonical main。

**Architecture:** 继续优先走 tests-first。direct provider 与 installer runtime 各补一组 `.bak` fallback corruption contracts，完全复用现有 binary fixture helpers、installer context helper、以及 resumed early-data accepted/rejected runtime assertion helpers。只有 fresh RED 明确落到 provider drift，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`；默认不重开 public API、builder / factory、managed seam、context / connection wiring 与 capability wording。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalStoreBackedEarlyDataReplayProvider`, file-backed replay store, TLS 1.3 scripted runtime tests, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先补 2026-04-16 `.bak` fallback corruption hardening` 计划与 working-memory 入口。
2. 在 `tests/test_freepascal_tls13_early_data.pas` 先补 direct provider 的 `.bak` fallback corruption contracts。
3. 再把同一组语义提升到 installer runtime resumed early-data `Accept` path。
4. 跑 focused / adjacent / completeness / compile / hygiene，并用 fresh evidence 回填 roadmap / findings / progress / task_plan。

## Task 1: RED - Lock `.bak` fallback corruption fail-closed semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Add direct provider contracts**
- 先用正常 file-backed provider materialize 一份 canonical main replay truth。
- 再把可读路径切到 `.bak` fallback：
  - `main` 删除
  - `.tmp` 缺失
  - `.bak` 存在
- 对 `.bak` 依次构造四类 fixture：
  - invalid version
  - truncated payload
  - oversize entry count
  - oversize key length
- 固定断言：
  - fresh blocked session acquire 返回 `False`
  - canonical main 继续缺失
  - `.tmp` 不残留
  - corrupt `.bak` 保留
  - 同一 original replay session 也不会被误放行

**Step 2: Add runtime contracts**
- 先用正常 installer file-backed path materialize 一份 canonical main replay truth。
- 再把 runtime store file 切到 `.bak` fallback corruption 状态：
  - `main` 删除
  - `.tmp` 缺失
  - `.bak` 写入对应 corrupt fixture
- 固定断言：
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

**Step 1: Re-run focused test and inspect fresh RED**
- Expected:
  - 若当前 provider 已天然让 `.bak` fallback 复用 `LoadEntries(...)` 的 corruption fail-closed 语义，则 focused suite 直接 GREEN
  - 若 fresh RED 明确说明 `.bak` fallback 漏掉了这套校验，才允许最小修补 readable resolution / load path

**Step 2: Bound any fallback fix**
- 只允许最小查看并修改：
  - `ResolveReadableStoreFileName(...)`
  - `LoadEntries(...)`
- 不改 `SaveEntries(...)` 提交流程，不改 installer/public/runtime wiring。

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
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH"; bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_bak_fallback_corruption_hardening_20260416`
- `python3 scripts/compile_all_modules.py`

**Step 2: Record evidence**
- 更新 roadmap / working-memory，明确：
  - `.bak` fallback corruption 是否继续 fail closed
  - runtime resumed early-data path 是否与 direct provider truth 对齐
  - 本批是否保持 tests-only，或 provider 最小修复落在哪
  - next queue 是否继续保持为更重的 durability / persistence family

**Step 3: Diff hygiene**
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-bak-fallback-corruption-hardening.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas task_plan.md findings.md progress.md`
