# FreePascal Early-Data Anti-Replay Builder Opt-In Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改变默认 shipped behavior 和 capability wording 的前提下，为 FreePascal TLS 1.3 early-data file-backed anti-replay seam 增加最小 public builder/config opt-in。

**Architecture:** 继续复用已经稳定的 backend-private seam：`IFreePascalContextEarlyDataReplayInstaller` + `TFreePascalContext.InstallFileBackedReplayLedger(...)`。builder 只新增一个 server-only file path 字段，并在 `BuildServer` 上薄接到该 seam；JSON/INI import-export、clone/reset/merge/override 同步接线，但不引入新的 persistence/provider abstraction，也不改变默认 in-memory ledger。

**Tech Stack:** FreePascal (ObjFPC), `TSSLContextBuilder`, FreePascal early-data replay seam, builder/config contract tests, scripted TLS 1.3 early-data runtime tests, file-based working memory.

---

## Summary

- 当前真实缺口已经从“缺少 anti-replay seam”收缩到“public builder/config 还没有 opt-in 到现有 seam”。
- 现有稳定真值已经具备：
  - `IFreePascalEarlyDataReplayLedgerAccess`
  - `IFreePascalContextEarlyDataReplayInstaller`
  - `TFreePascalContext.InstallFileBackedReplayLedger(...)`
  - helper wrapper `InstallFileBackedReplayLedger(...)`
- 本批只做最小 public builder/config 接线：
  - 新增 server-only builder method：`WithServerEarlyDataReplayStoreFile(...)`
  - `BuildServer` 若配置该字段，则要求 backend 支持 installer seam
  - JSON / INI import-export 与 clone/reset/merge/override 同步可见
- 本批明确不做：
  - 默认行为改变
  - capability wording 变化
  - distributed / multi-process consistency 承诺
  - 更重的 provider / callback redesign

## Delivery Order

1. 写 plan 和 working-memory 入口，锁定 scope。
2. 在 runtime/config/clone surfaces 上补 RED。
3. 最小实现只改 builder plumbing。
4. 跑 focused GREEN、adjacent regressions、completeness gate、compile gate。
5. 回填 findings / progress / task plan。

### Task 1: Add RED Contracts For Builder Opt-In

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Modify: `tests/config/test_config_import_export.pas`
- Modify: `tests/config/test_config_snapshot_clone.pas`

**Step 1: Add runtime builder contract**
- 在 `tests/test_freepascal_tls13_early_data.pas`：
  - 增加 builder-built server contexts helper
  - 新增 cross-context runtime contract，覆盖：
    - 两个 builder-built FreePascal server contexts 配置同一个 replay store file
    - 第一次 resumed early-data accept 成功
    - 第二次跨 context resumed early-data 被 reject
    - resumed handshake 仍成功、session 仍 reused、early bytes 不可读

**Step 2: Add config round-trip contract**
- 在 `tests/config/test_config_import_export.pas`：
  - 为新字段增加 JSON round-trip 可见性
  - 为新字段增加 INI round-trip 可见性
  - key 固定为 `server_early_data_replay_store_file`

**Step 3: Add clone/reset/merge contract**
- 在 `tests/config/test_config_snapshot_clone.pas`：
  - clone 保留新字段
  - reset 清空新字段
  - merge 传播新字段

**Step 4: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `mkdir -p tmp/test_config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_import_export -FEtmp/test_config_import_export -otmp/test_config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export/test_config_import_export`
  - `mkdir -p tmp/test_config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_snapshot_clone -FEtmp/test_config_snapshot_clone -otmp/test_config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone/test_config_snapshot_clone`
- Expected:
  - FAIL，集中暴露：
    - builder 缺少 `WithServerEarlyDataReplayStoreFile(...)`
    - export/import/clone/reset/merge/override 尚未识别 `server_early_data_replay_store_file`

### Task 2: Implement Minimal Builder/Config Opt-In

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Add builder field and fluent method**
- 新增 backing field：
  - `FServerEarlyDataReplayStoreFile: string`
- 新增 fluent method：
  - `WithServerEarlyDataReplayStoreFile(const AFile: string): ISSLContextBuilder`

**Step 2: Wire BuildServer**
- `BuildServer` 上：
  - 若字段为空，不改变现有行为
  - 若字段非空：
    - 要求 context 支持 `IFreePascalContextEarlyDataReplayInstaller`
    - 调用 `InstallFileBackedReplayLedger(...)`
    - seam 缺失或安装失败时，抛出清晰配置错误

**Step 3: Wire config surfaces**
- 接入：
  - `ExportToJSON`
  - `ImportFromJSON`
  - `ExportToINI`
  - `ImportFromINI`
  - `Clone`
  - `Reset`
  - `Merge`
  - `Override`
- key 固定为：
  - `server_early_data_replay_store_file`

### Task 3: Verify And Record

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run focused GREEN**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `mkdir -p tmp/test_config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_import_export -FEtmp/test_config_import_export -otmp/test_config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export/test_config_import_export`
  - `mkdir -p tmp/test_config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_snapshot_clone -FEtmp/test_config_snapshot_clone -otmp/test_config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone/test_config_snapshot_clone`

**Step 2: Run adjacent regressions**
- Run:
  - `mkdir -p tmp/test_context_builder_early_data_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_early_data_contract -FEtmp/test_context_builder_early_data_contract -otmp/test_context_builder_early_data_contract/test_context_builder_early_data_contract tests/config/test_context_builder_early_data_contract.pas && ./tmp/test_context_builder_early_data_contract/test_context_builder_early_data_contract`
  - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`

**Step 3: Run gates**
- Run:
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_antireplay_builder_optin_20260413`
  - `python3 scripts/compile_all_modules.py`

**Step 4: Diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-13-freepascal-early-data-antireplay-builder-optin.md src/fafafa.ssl.context.builder.pas tests/test_freepascal_tls13_early_data.pas tests/config/test_config_import_export.pas tests/config/test_config_snapshot_clone.pas task_plan.md findings.md progress.md`

### Definition Of Done

- builder 提供 server-side file-backed replay store opt-in
- `BuildServer` 通过现有 backend-private installer seam 完成装配
- JSON / INI / clone / reset / merge / override 都能表达新字段
- builder-built FreePascal server contexts 共享 replay store file 时，跨 context replay 继续被拒绝
- capability wording 继续保持 `experimental + in-memory single-process anti-replay ledger`
- focused regressions、focused gate、compile gate 与 diff hygiene 通过
