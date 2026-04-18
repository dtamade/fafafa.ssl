# FreePascal Early-Data Directory-Store Public Opt-In Parity Implementation Plan

**Goal:** 在不改变默认 shipped behavior、capability wording、默认 in-memory single-process anti-replay truth 的前提下，把已经稳定的 directory-backed replay store 暴露为最小 public opt-in：builder / `TSSLConfig` / `TSSLFactory` 都能配置 directory-backed early-data replay store，并保持 clear error contracts。

**Architecture:** 继续保持 existing file-backed public path 与 backend-private directory-store 实现分层不变。新增一条 backend-private context installer seam 只负责按目录路径装配 `TFreePascalDirectoryEarlyDataReplayStore`；builder / factory 只薄接这个 seam，不直接实现 store/provider 逻辑。public shape 增加一个 server-only directory path 字段；当 `server_early_data_replay_store_file` 与 `server_early_data_replay_store_directory` 同时配置时，builder / factory 明确 fail fast，不做隐式优先级。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data public opt-in wiring, `TSSLContextBuilder`, `TSSLConfig`, `TSSLFactory`, backend-private FreePascal installer seams, focused config/runtime tests.

---

## Summary

1. 先补 RED，覆盖 builder/config/factory 对 directory-store public opt-in 的最小合同。
2. 重点锁三类 truth：
   - config/import-export/clone/reset/merge/normalize 可见性
   - builder/factory/runtime 能真正装上 directory-backed replay store
   - file-path 与 directory-path 同时配置时 fail fast，而不是 silent precedence
3. 最小 GREEN 只允许薄接现有 stable directory-store，不重开 default path、capability wording 或更深 durability 分支。

## Task 1: RED - Lock Public Config Surface

**Files:**
- Modify: `tests/config/test_config_import_export.pas`
- Modify: `tests/config/test_config_snapshot_clone.pas`
- Modify: `tests/test_factory_logic.pas`

**Step 1: Add config visibility contracts**
- 新增 JSON / INI round-trip 合同：
  - key: `server_early_data_replay_store_directory`
- 新增 clone / reset / merge 合同：
  - clone preserves directory field
  - reset clears directory field
  - merge preserves directory field
- 新增 raw config / normalize 合同：
  - `TSSLConfig.ServerEarlyDataReplayStoreDirectory` 默认为空串
  - `NormalizeConfig(...)` 不清空该字段

## Task 2: RED - Lock Builder/Factory Error Contracts

**Files:**
- Modify: `tests/config/test_context_builder_try.pas`
- Modify: `tests/test_factory_config_early_data_isolation.pas`

**Step 1: Add builder error contracts**
- backend 缺 directory installer seam => clear error
- installer 返回 `False` => clear error
- file + directory 同时配置 => clear conflict error

**Step 2: Add factory error/isolation contracts**
- one-shot config 同时配置 file + directory => `TSSLFactory.CreateContext(...)` raise clear configuration error
- default config / one-shot config 能装配 directory-backed replay store
- one-shot directory config 不泄漏到 shared defaults

## Task 3: RED - Lock Real Runtime Wiring

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Add builder/factory runtime contracts**
- 新增 builder-built directory-store helper
- 新增 factory-built directory-store helper
- focused runtime 合同覆盖：
  - builder-built context first accepts resumed early-data through directory-backed replay truth
  - factory-built context rejects replay from the same directory-backed truth
  - mirrored factory-first / builder-second path
- 不重开 directory-store durability family；只验证 public opt-in wiring

## Task 4: GREEN - Implement Minimal Wiring

**Files:**
- Modify: `src/fafafa.ssl.base.pas`
- Modify: `src/fafafa.ssl.context.builder.pas`
- Modify: `src/fafafa.ssl.factory.pas`
- Modify: `src/fafafa.ssl.pas`
- Modify: `src/fafafa.ssl.debug.utils.pas`
- Modify: `src/fafafa.ssl.freepascal.context.material.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas`

**Step 1: Extend public config/builder**
- `TSSLConfig` 新增 `ServerEarlyDataReplayStoreDirectory: string`
- builder 新增 `WithServerEarlyDataReplayStoreDirectory(...)`
- JSON / INI / clone / reset / merge / override 接线新字段

**Step 2: Add backend-private directory installer seam**
- 在 `context.material` 新增 directory installer interface
- `TFreePascalContext` 实现：
  - validate non-empty directory path
  - create `TFreePascalDirectoryEarlyDataReplayStore`
  - install via existing provider-backed ledger path

**Step 3: Wire builder/factory fail-fast logic**
- 若 file + directory 同时配置：
  - builder `BuildServer` fail fast
  - factory `ApplyEarlyDataReplayStoreConfig(...)` fail fast
- 若只配 directory：
  - builder 通过 directory installer seam 装配
  - factory 通过 directory installer seam 装配

## Task 5: Verify And Close Out

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run focused verification**
- Run:
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/test_config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_import_export -FEtmp/test_config_import_export -otmp/test_config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export/test_config_import_export`
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/test_config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_snapshot_clone -FEtmp/test_config_snapshot_clone -otmp/test_config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone/test_config_snapshot_clone`
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/test_context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_try -FEtmp/test_context_builder_try -otmp/test_context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/test_context_builder_try/test_context_builder_try`
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/test_factory_logic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logic -FEtmp/test_factory_logic -otmp/test_factory_logic/test_factory_logic tests/test_factory_logic.pas && ./tmp/test_factory_logic/test_factory_logic`
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/test_factory_config_early_data_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_config_early_data_isolation -FEtmp/test_factory_config_early_data_isolation -otmp/test_factory_config_early_data_isolation/test_factory_config_early_data_isolation tests/test_factory_config_early_data_isolation.pas && ./tmp/test_factory_config_early_data_isolation/test_factory_config_early_data_isolation`
  - `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_public_optin_parity_20260419`
  - `python3 scripts/compile_all_modules.py`
  - `git diff --check -- docs/plans/2026-04-19-freepascal-early-data-directory-store-public-optin-parity.md src/fafafa.ssl.base.pas src/fafafa.ssl.context.builder.pas src/fafafa.ssl.factory.pas src/fafafa.ssl.pas src/fafafa.ssl.debug.utils.pas src/fafafa.ssl.freepascal.context.material.pas src/fafafa.ssl.freepascal.context.pas tests/config/test_config_import_export.pas tests/config/test_config_snapshot_clone.pas tests/config/test_context_builder_try.pas tests/test_factory_logic.pas tests/test_factory_config_early_data_isolation.pas tests/test_freepascal_tls13_early_data.pas docs/ROADMAP.md task_plan.md findings.md progress.md`

### Definition Of Done

- builder / `TSSLConfig` / `TSSLFactory` 都能表达 directory-backed replay store opt-in
- FreePascal context 提供 backend-private directory installer seam
- builder/factory public path 真正装上 directory-backed replay ledger，并在 resumed early-data runtime 上共享 replay truth
- file + directory 双配置明确 fail fast
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
