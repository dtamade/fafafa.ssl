# FreePascal Early-Data Replay-Store Factory Parity And Error Contracts Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改变默认 shipped behavior 和 capability wording 的前提下，把 FreePascal TLS 1.3 early-data replay-store file opt-in 从 builder 扩到 `TSSLConfig` / `TSSLFactory` 路径，并补齐 builder/factory 的 clear error contracts。

**Architecture:** 继续复用已经稳定的 backend-private seam：`IFreePascalContextEarlyDataReplayInstaller`。`TSSLConfig` 只新增一个 server-only replay-store file 字段，默认空串，表示继续使用默认 in-memory ledger；`TSSLFactory` 成为统一的 config 应用层，在 context 支持 installer seam 时薄接安装 file-backed replay ledger。builder/factory 的错误语义保持 fail-closed：要求 installer seam 时给出清晰配置错误，installer 返回失败时也给出清晰配置错误。

**Tech Stack:** FreePascal (ObjFPC), `TSSLConfig`, `TSSLFactory`, `TSSLContextBuilder`, FreePascal backend-private replay installer seam, focused builder/factory contract tests, scripted TLS 1.3 early-data runtime tests, file-based working memory.

---

## Summary

- 当前 live truth：
  - builder 已支持 `WithServerEarlyDataReplayStoreFile(...)`
  - runtime cross-context replay rejection 已被 builder-path 测试锁住
  - `TSSLConfig` / `TSSLFactory` 已支持 early-data 三元组，但还不能表达 replay-store file opt-in
  - `docs/ROADMAP.md` 仍把 replaceable/persistent seam 当成“下一条最值得开的实现线”，已经陈旧
- 本批只做最小高 ROI 收口：
  - `TSSLConfig` 新增 `ServerEarlyDataReplayStoreFile`
  - `CreateDefaultConfig(...)` 默认值暴露为空串
  - `TSSLFactory.CreateContext(...)` 两个 overload 都应用 replay-store file
  - builder / factory error path 锁定 clear failure wording
  - roadmap / guide 改成“seam + file provider prototype + builder opt-in 已收口，下一步是 factory/config parity closeout”
- 本批明确不做：
  - capability wording 升级
  - distributed / multi-process anti-replay 承诺
  - 新的 provider/callback public abstraction

## Delivery Order

1. 写计划并记录 working-memory 入口。
2. 先补 RED：`TSSLConfig` replay-store field、factory parity、builder/factory negative-path contracts。
3. 最小 GREEN：只改 `src/fafafa.ssl.base.pas`、`src/fafafa.ssl.factory.pas`、`src/fafafa.ssl.pas`、`src/fafafa.ssl.debug.utils.pas`，仅在必要时微调测试 mock。
4. 跑 focused tests、factory tests、completeness gate、compile gate、diff hygiene。
5. 回写 `task_plan.md` / `findings.md` / `progress.md`，最后收 docs。

### Task 1: Add RED Contracts For Replay-Store Factory Parity And Error Paths

**Files:**
- Modify: `tests/config/test_default_config.pas`
- Modify: `tests/test_factory_logic.pas`
- Modify: `tests/test_factory_config_early_data_isolation.pas`
- Modify: `tests/test_context_builder_try.pas`

**Step 1: Extend default-config contract**
- 在 `tests/config/test_default_config.pas`：
  - 断言 `CreateDefaultConfig(...)` 默认 `ServerEarlyDataReplayStoreFile = ''`

**Step 2: Extend raw config record contract**
- 在 `tests/test_factory_logic.pas`：
  - 断言 `TSSLConfig.ServerEarlyDataReplayStoreFile` 可读写
  - 断言 `NormalizeConfig(...)` 不会清空该字段

**Step 3: Extend factory isolation contract**
- 在 `tests/test_factory_config_early_data_isolation.pas`：
  - default-config path：
    - `ISSLLibrary.SetDefaultConfig(...)` 指定 replay-store file
    - 两个 `TSSLFactory.CreateContext(sslCtxServer, sslFreePascal)` 指向同一个 file
    - 第一条 resumed early-data accept 成功
    - 第二条跨 context replay 被 reject，但 resumed handshake 继续成功
  - one-shot path：
    - `TSSLFactory.CreateContext(const AConfig)` 应用 replay-store file
    - 后续 default-path context 不继承 one-shot replay-store file

**Step 4: Add builder negative-path contracts**
- 在 `tests/test_context_builder_try.pas`：
  - 使用 mock backend/context 覆盖：
    - 配置 `server_early_data_replay_store_file` 但 context 不支持 installer seam => `TryBuildServer` 返回 Err，错误文案含 `requires a backend`
    - context 支持 installer seam 但 `InstallFileBackedReplayLedger(...)` 返回 `False` => `TryBuildServer` 返回 Err，错误文案含 `could not install`

**Step 5: Run RED**
- Run:
  - `mkdir -p tmp/default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/default_config -FEtmp/default_config -otmp/default_config/test_default_config tests/config/test_default_config.pas && ./tmp/default_config/test_default_config`
  - `mkdir -p tmp/factory_logic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_logic -FEtmp/factory_logic -otmp/factory_logic/test_factory_logic tests/test_factory_logic.pas && ./tmp/factory_logic/test_factory_logic`
  - `mkdir -p tmp/factory_config_early_data_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_config_early_data_isolation -FEtmp/factory_config_early_data_isolation -otmp/factory_config_early_data_isolation/test_factory_config_early_data_isolation tests/test_factory_config_early_data_isolation.pas && ./tmp/factory_config_early_data_isolation/test_factory_config_early_data_isolation`
  - `mkdir -p tmp/test_context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_try -FEtmp/test_context_builder_try -otmp/test_context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/test_context_builder_try/test_context_builder_try`
- Expected:
  - RED 至少暴露：
    - `TSSLConfig` 缺少 `ServerEarlyDataReplayStoreFile`
    - factory path 未应用 replay-store file config

### Task 2: Implement Minimal Replay-Store Factory Parity

**Files:**
- Modify: `src/fafafa.ssl.base.pas`
- Modify: `src/fafafa.ssl.factory.pas`
- Modify: `src/fafafa.ssl.pas`
- Modify: `src/fafafa.ssl.debug.utils.pas`

**Step 1: Extend public config record**
- 在 `src/fafafa.ssl.base.pas`：
  - `TSSLConfig` 增加 `ServerEarlyDataReplayStoreFile: string`

**Step 2: Keep defaults and diagnostics aligned**
- 在 `src/fafafa.ssl.pas`：
  - `CreateDefaultConfig(...)` 默认/异常 fallback 都保留空串 replay-store file
- 在 `src/fafafa.ssl.debug.utils.pas`：
  - `DumpSSLConfig(...)` 输出 replay-store file

**Step 3: Apply replay-store config in factory**
- 在 `src/fafafa.ssl.factory.pas`：
  - 新增内部 helper，把 `ServerEarlyDataReplayStoreFile` 应用到 `IFreePascalContextEarlyDataReplayInstaller`
  - 两个 `CreateContext(...)` overload 都调用该 helper
  - 语义保持：
    - 空串 => no-op
    - seam 缺失 => clear configuration error
    - installer 返回 `False` => clear configuration error
    - one-shot config 只影响返回的 context，不改 shared defaults

**Step 4: Run GREEN**
- Re-run Task 1 commands
- Expected:
  - PASS

### Task 3: Close Out Docs And Working Memory

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `docs/INTEGRATION_GUIDE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Update roadmap truth**
- 在 `docs/ROADMAP.md`：
  - 移除“replaceable / persistent anti-replay seam 仍待推进”的陈旧 next queue
  - 改成 builder opt-in 已收口、当前 closeout 是 factory/config replay-store parity
  - capability wording 仍保持 experimental / in-memory single-process caveat

**Step 2: Add minimal usage note**
- 在 `docs/INTEGRATION_GUIDE.md`：
  - 给 early-data 相关段落补一小段 server-side replay-store file opt-in 示例或说明
  - 明确它是 FreePascal server-only opt-in，不改变默认 in-memory shipped path

**Step 3: Run verification**
- Run:
  - `mkdir -p tmp/default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/default_config -FEtmp/default_config -otmp/default_config/test_default_config tests/config/test_default_config.pas && ./tmp/default_config/test_default_config`
  - `mkdir -p tmp/factory_logic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_logic -FEtmp/factory_logic -otmp/factory_logic/test_factory_logic tests/test_factory_logic.pas && ./tmp/factory_logic/test_factory_logic`
  - `mkdir -p tmp/factory_config_early_data_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/factory_config_early_data_isolation -FEtmp/factory_config_early_data_isolation -otmp/factory_config_early_data_isolation/test_factory_config_early_data_isolation tests/test_factory_config_early_data_isolation.pas && ./tmp/factory_config_early_data_isolation/test_factory_config_early_data_isolation`
  - `mkdir -p tmp/test_context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_try -FEtmp/test_context_builder_try -otmp/test_context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/test_context_builder_try/test_context_builder_try`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_replay_store_factory_parity_20260413`
  - `python3 scripts/compile_all_modules.py`

**Step 4: Diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-13-freepascal-early-data-replay-store-factory-parity-and-error-contracts.md src/fafafa.ssl.base.pas src/fafafa.ssl.factory.pas src/fafafa.ssl.pas src/fafafa.ssl.debug.utils.pas tests/config/test_default_config.pas tests/test_factory_logic.pas tests/test_factory_config_early_data_isolation.pas tests/config/test_context_builder_try.pas docs/ROADMAP.md docs/INTEGRATION_GUIDE.md task_plan.md findings.md progress.md`

### Definition Of Done

- `TSSLConfig` 可表达 `ServerEarlyDataReplayStoreFile`
- `CreateDefaultConfig(...)` 默认值与 builder shipped behavior 对齐
- `TSSLFactory.CreateContext(...)` 两个 overload 都能应用 replay-store file opt-in
- builder negative-path / factory parity contracts 有 fresh tests
- capability wording 继续保持 `experimental + in-memory single-process anti-replay ledger`
- focused regressions、completeness gate、compile gate、diff hygiene 通过
- working-memory files 与 docs 记录当前 closeout truth
