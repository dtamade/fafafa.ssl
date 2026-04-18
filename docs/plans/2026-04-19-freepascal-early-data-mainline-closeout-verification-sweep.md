# FreePascal Early-Data Mainline Closeout Verification Sweep Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不重开新持久化形态、不改变默认 shipped behavior、public surface、capability wording 的前提下，对 FreePascal TLS 1.3 early-data 主线做一轮权威 verification sweep，确认当前主线已收口到“仅在 fresh RED 出现时再重开”。

**Architecture:** 这批优先把权威真相源固定在 `docs/ROADMAP.md`、`README.md`、`docs/reference/API_REFERENCE.md`、focused Pascal suites 与 completeness gate 上。执行顺序是先跑 docs contract、factory/config isolation、runtime early-data focused suite、capability wording focused suite，再跑 completeness gate；只有 fresh RED 明确出现时，才允许最小修复直接命中的 tests/docs/public wiring/replay-store seam。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data focused suites, docs contract shell test, completeness gate, file-based working memory.

---

## Summary

1. 先确认 authoritative truth 是否已经一致：
   - default shipped path 继续是 `in-memory single-process anti-replay ledger`
   - public opt-in 继续是 file/directory 两条路径
   - `file` / `directory` 继续 mutually exclusive
2. 先跑现有护栏，不预设要改代码。
3. 只有 fresh RED 才允许最小修复；如果所有护栏全绿，则本批收口为 verification-only closeout。

## Task 1: Reconfirm Authoritative Truth

**Files:**
- Reference: `docs/ROADMAP.md`
- Reference: `README.md`
- Reference: `docs/reference/API_REFERENCE.md`
- Reference: `src/fafafa.ssl.base.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`
- Reference: `src/fafafa.ssl.factory.pas`

**Step 1: Reconfirm default capability wording**
- 确认 `docs/ROADMAP.md` 仍写明：
  - `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`

**Step 2: Reconfirm public opt-ins**
- 确认 public surface 仍是：
  - `TSSLConfig.ServerEarlyDataReplayStoreFile`
  - `TSSLConfig.ServerEarlyDataReplayStoreDirectory`
  - `WithServerEarlyDataReplayStoreFile(...)`
  - `WithServerEarlyDataReplayStoreDirectory(...)`
- 确认 builder/factory 双配置保持 mutually exclusive fail-fast。

## Task 2: Run Focused Verification First

**Files:**
- Test: `tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
- Test: `tests/test_factory_config_early_data_isolation.pas`
- Test: `tests/test_freepascal_tls13_early_data.pas`
- Test: `tests/test_capability_cache.pas`

**Step 1: Run docs contract**
- Run:
  - `bash tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
- Expected:
  - PASS；若失败，缺口应落在权威 README / API reference wording

**Step 2: Run factory/config isolation contract**
- Run:
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/test_factory_config_early_data_isolation_mainline_sweep && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_config_early_data_isolation_mainline_sweep -FEtmp/test_factory_config_early_data_isolation_mainline_sweep -otmp/test_factory_config_early_data_isolation_mainline_sweep/test_factory_config_early_data_isolation tests/test_factory_config_early_data_isolation.pas && ./tmp/test_factory_config_early_data_isolation_mainline_sweep/test_factory_config_early_data_isolation`
- Expected:
  - PASS；默认/one-shot file+directory public opt-in parity 继续成立

**Step 3: Run runtime early-data focused suite**
- Run:
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/freepascal_tls13_early_data_mainline_sweep && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data_mainline_sweep -FEtmp/freepascal_tls13_early_data_mainline_sweep -otmp/freepascal_tls13_early_data_mainline_sweep/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data_mainline_sweep/test_freepascal_tls13_early_data`
- Expected:
  - PASS；若失败，才允许重开直接命中的 family

**Step 4: Run capability wording alignment suite**
- Run:
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/capability_cache_mainline_sweep && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache_mainline_sweep -FEtmp/capability_cache_mainline_sweep -otmp/capability_cache_mainline_sweep/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache_mainline_sweep/test_capability_cache`
- Expected:
  - PASS；KnownIssues wording 与 capability cache 继续对齐

## Task 3: Run Mainline Gate

**Files:**
- Script: `scripts/run_freepascal_tls13_completeness_gate.sh`

**Step 1: Run completeness gate**
- Run:
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_mainline_closeout_verification_sweep_20260419`
- Expected:
  - PASS；若 focused suites 全绿且 gate 也绿，本批直接收口为 verification-only closeout

## Task 4: Fix Only If Fresh RED Appears

**Files:**
- Modify only if directly required:
  - `README.md`
  - `docs/reference/API_REFERENCE.md`
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.context.builder.pas`
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.freepascal.context.pas`
  - `src/fafafa.ssl.freepascal.context.material.pas`
  - `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
  - `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Step 1: Keep fix surface minimal**
- 若 docs contract 红，只修权威 wording。
- 若 factory/config contract 红，只修 public wiring。
- 若 runtime suite 红，只修直接命中的 harness/replay-store seam。
- 明确不做：
  - SQLite
  - distributed anti-replay
  - 新 provider redesign
  - 无 RED 重开旧 family

## Task 5: Close Out

**Files:**
- Modify: `docs/ROADMAP.md` only if wording drifts
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record current truth**
- 记录本批是 verification-only closeout，或者记录 fresh RED 与最小修复范围。

**Step 2: Review and commit**
- 若有 repo-tracked 改动：
  - 跑 `git diff --check -- <scoped files>`
  - 只 stage scoped hunks
  - commit
- 若无 repo-tracked 改动：
  - 不制造无意义代码改动；只回写 gitignored working-memory，并报告“本批验证收口，无需新生产提交”

### Definition Of Done

- docs contract、factory/config isolation、runtime early-data focused suite、capability wording alignment 全部 fresh PASS
- completeness gate fresh PASS
- authoritative truth 保持一致：
  - default shipped path 仍是 `in-memory single-process anti-replay ledger`
  - public opt-in 仍是 file/directory 两条路径
  - `file` / `directory` 仍 mutually exclusive
- 没有 fresh RED 时，不新增 `src/` 改动，不重开新 persistence family
