# FreePascal Early-Data Permission / Write-Failure Shapes Implementation Plan

**Goal:** 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 file-backed replay-store 再补一批更真实的 permission/write-failure 形态验证：`.tmp` 打开/写入被拒绝时继续 fail closed 且保住既有 truth；existing-main replace fallback 上若 `main -> .bak` 提升失败，也继续 fail closed、不丢 canonical main truth。

**Architecture:** 继续优先走 tests-first，并且优先用 deterministic hook injection，而不是 chmod / 平台权限位。`tests/test_freepascal_tls13_early_data.pas` 会新增两个最小 scripted file-backed store 子类：一个只在 `.tmp` 写入路径上拒绝 `OpenWriteFileStream(...)`，另一个只在 existing-main replace fallback 上拒绝 `main -> .bak`。direct provider 与 runtime resumed early-data `Accept` path 各补对应合同。只有 fresh RED 明确落到 provider drift，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`；默认不重开 public API、builder / factory / installer seam、capability wording、或分布式/持久化范围。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalStoreBackedEarlyDataReplayProvider`, file-backed replay store, TLS 1.3 scripted runtime tests, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先补一份 2026-04-16 permission/write-failure batch 计划，并把 working-memory 的 next queue 对齐到 deterministic denial 形态。
2. 在 `tests/test_freepascal_tls13_early_data.pas` 先加两个 scripted file-backed store 子类。
3. 先补 direct provider 的 `.tmp` open denied / backup-promotion denied 合同。
4. 再把同一组语义提升到 runtime resumed early-data `Accept` path。
5. 跑 focused / adjacent / completeness / compile / hygiene，并用 fresh evidence 回填 roadmap / findings / progress / task_plan。

## Task 1: RED - Lock deterministic permission/write-failure shapes

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Add a scripted temp-write denied store**
- 在现有 scripted file-backed store 旁边新增一个最小 subclass。
- 子类继续只 override file-op wrappers，不重写 `SaveEntries(...)`。
- 脚本化语义固定为：
  - `OpenWriteFileStream(<store>.tmp)` => raise deterministic denial
  - 其余路径全部 inherited

**Step 2: Add a direct temp-write denied contract**
- 先用正常 file-backed provider materialize 既有 session A 的 canonical main truth。
- 再用 scripted temp-write denied store 对 fresh session B 触发 denied 写入。
- 断言：
  - B acquire fail closed
  - canonical main 继续存在
  - canonical main bytes 保持不变
  - `.tmp` 不残留
  - `.bak` 不产生
  - scripted ledger 对 A 立即继续 reject
  - normal provider rebuild 后，A 继续 reject，B first accept，然后 replay reject

**Step 3: Add a scripted backup-promotion denied store**
- 在现有 scripted file-backed store 旁边新增第二个最小 subclass。
- 子类继续只 override `RenameFileAt(...)`。
- 脚本化语义固定为：
  - `temp -> main` => fail
  - `main -> .bak` => fail
  - 其余 rename paths inherited

**Step 4: Add a direct backup-promotion denied contract**
- 先用正常 file-backed provider materialize 既有 session A 的 canonical main truth。
- 再用 scripted backup-promotion denied store 对 fresh session B 触发 existing-main replace fallback denial。
- 断言：
  - B acquire fail closed
  - canonical main 继续存在
  - canonical main bytes 保持不变
  - `.tmp` cleaned
  - `.bak` 不存在
  - scripted ledger 对 A 立即继续 reject
  - normal provider rebuild 后，A 继续 reject，B first accept，然后 replay reject

**Step 5: Add runtime contracts**
- temp-write denied runtime path：
  - 先用正常 installer file-backed path materialize A
  - 再用 `InstallStoreBackedReplayLedger(...)` 把 scripted temp-write denied store 装到真实 server context
  - 断言 B replay fail closed、A 继续 reject、canonical main bytes unchanged、`.tmp` absent、`.bak` absent
  - normal installer rebuild 后，A reject、B accept、B replay reject
- backup-promotion denied runtime path：
  - 先用正常 installer file-backed path materialize A
  - 再用 `InstallStoreBackedReplayLedger(...)` 把 scripted backup-promotion denied store 装到真实 server context
  - 断言 B replay fail closed、A 继续 reject、canonical main bytes unchanged、`.tmp` absent、`.bak` absent
  - normal installer rebuild 后，A reject、B accept、B replay reject

## Task 2: GREEN - Keep provider changes strictly optional

**Files:**
- Default: no `src/` changes
- Fallback Modify: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Run focused suite and inspect fresh RED**
- Expected:
  - 如果当前 provider 已天然满足这些 deterministic denial 形态，focused suite 直接 GREEN
  - 如果 fresh RED 明确指出 provider drift，只允许最小查看 `SaveEntries(...)` 或直接耦合的 file-op cleanup 行为

**Step 2: Keep any fallback fix bounded**
- 只允许最小查看并修改：
  - `TFreePascalFileEarlyDataReplayStore.SaveEntries(...)`
  - 与 temp open denied / existing-main backup-promotion denial directly coupled 的 provider internals
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
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH"; bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_permission_write_failure_shapes_20260416`
- `python3 scripts/compile_all_modules.py`

**Step 2: Record evidence**
- 更新 roadmap / working-memory，明确：
  - deterministic `.tmp` open denied 是否继续 preserve existing truth + fail closed
  - deterministic `main -> .bak` promotion denied 是否继续 preserve canonical main truth + fail closed
  - 本批是否保持 tests-only，或 provider 最小修复具体落点
  - next queue 是否继续保持为更重但仍 bounded 的 permission/write-failure / persistence 形态
  - capability wording 保持不变

**Step 3: Diff hygiene**
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-permission-write-failure-shapes.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas task_plan.md findings.md progress.md`
