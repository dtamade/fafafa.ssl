# FreePascal Early-Data Public Opt-In Runtime Durability Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改 capability wording、默认 shipped behavior、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 FreePascal early-data 的 public opt-in 入口补齐一层真实 runtime durability evidence，优先锁住 builder 与 `TSSLFactory.CreateContext(const AConfig)` 这两条路径。

**Architecture:** 继续复用现有 resumed early-data scripted runtime harness、`CaptureServerIssuedSession(...)`、`WriteBytesToFile(...)` / `ReadBytesFromFile(...)`、以及现有 child self-exec `RunReplayProviderRuntimeReplayProbeMode(...)`。父进程改为通过 public builder/config 入口创建 server context，并让 public path 写出 file-backed replay truth；子进程继续复用现有 backend-private installer replay-probe helper，验证 persisted truth 穿过进程边界后仍 reject replay，同时 fresh resumed session 仍 accept。只有 fresh RED 明确落在 public config application 上时，才最小查看 `src/fafafa.ssl.factory.pas`。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data scripted runtime tests, `TSSLContextBuilder`, `TSSLFactory.CreateContext(const AConfig)`, file-backed replay-store opt-in, child-process self-exec, file-based working memory.

---

## Summary

- 当前 backend-private installer seam 已经有：
  - process restart durability
  - tiny repeated restart loop
  - crash-window restart durability
  - runtime lock-contention fail-closed
- 当前最高 ROI 的剩余缺口不是再碰底层 seam，而是：
  - 证明 public builder opt-in 写出的 replay truth 与 runtime restart probe 完全一致
  - 证明 one-shot factory config opt-in 也能走真实 resumed early-data runtime path，而不只是 direct ledger acquire
- 本批明确不做：
  - distributed / multi-host persistence
  - capability level 升级
  - managed seam / local gate 重新设计
  - `context.pas` / `connection.pas` 结构调整

## Delivery Order

1. 写本轮 plan，并把 `task_plan.md` / `findings.md` / `progress.md` 顶部切到 public opt-in runtime durability 批次。
2. 拉起 `gpt-5.4` 团队：一个 worker 只负责 `tests/test_freepascal_tls13_early_data.pas`，一个 reviewer 只看 public builder/factory runtime contract 的最小 shape。
3. 在 `tests/test_freepascal_tls13_early_data.pas` 先补 builder runtime durability RED。
4. 若仍然低风险，再补 one-shot `TSSLFactory.CreateContext(const AConfig)` runtime durability RED。
5. 跑 focused suite；只有 fresh RED 明确指向 public config application gap 时，才最小查看 `src/fafafa.ssl.factory.pas`。
6. 跑 adjacent regressions、completeness gate、compile gate、diff hygiene。
7. 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Task 1: RED - Lock Builder Runtime Durability

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`
- Reference: `src/fafafa.ssl.factory.pas`

**Step 1: Reuse existing restart primitives**
- 复用现有：
  - `BuildReplayProviderStoreFilePath(...)`
  - `BuildReplayProviderMarkerFilePath(...)`
  - `CleanupReplayProviderStoreFiles(...)`
  - `DeleteFileIfExists(...)`
  - `WriteBytesToFile(...)`
  - `RunReplayProviderRuntimeReplayProbeMode(...)`
  - `CaptureServerIssuedSession(...)`
  - `TCertificateUtils.TryGenerateSelfSignedSimple(...)`

**Step 2: Add a builder runtime durability contract**
- 新增 focused runtime contract，覆盖：
  - builder-built FreePascal server context 配置 file-backed replay store
  - first resumed early-data accept 成功
  - replay-store file 与 serialized session file 落盘
  - child replay probe 在新进程里对同一 session reject replay
  - child fresh resumed session 仍 accept

### Task 2: RED - Lock One-Shot Factory Config Runtime Durability

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.factory.pas`
- Reference: `src/fafafa.ssl.pas`
- Reference: `src/fafafa.ssl.base.pas`

**Step 1: Keep factory path minimal**
- 如果加入第二条合同，优先用：
  - `CreateDefaultConfig(sslCtxServer)`
  - `TSSLFactory.CreateContext(const AConfig)`
  - 临时 cert/key 文件
- 不新增 child mode；继续复用现有 replay probe helper

**Step 2: Add a one-shot config runtime durability contract**
- 新增 focused runtime contract，覆盖：
  - one-shot `TSSLConfig` 配置 file-backed replay-store file
  - one-shot factory-built server context 真实 accept 第一次 resumed early-data
  - child replay probe 在 restart 后 reject replay
  - fresh resumed session 仍 accept
  - one-shot path 不需要重开 shared default config leak 语义；那部分已有专门合同

### Task 3: GREEN - Only If Fresh RED Proves Public Path Drift

**Files:**
- Modify if needed: `src/fafafa.ssl.factory.pas`

**Step 1: Keep production scope minimal**
- 只有当 fresh RED 明确落在 public path config application 时，才最小查看：
  - `ApplyEarlyDataReplayStoreConfig(...)`
  - `TSSLFactory.CreateContext(const AConfig)`
- 不先碰 `context.pas` / `connection.pas` / file-provider unit

### Task 4: Verify Adjacent Truth Stays Locked

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Re-run focused and adjacent verification**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_public_optin_runtime_durability_20260414`
  - `python3 scripts/compile_all_modules.py`

**Step 2: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-public-optin-runtime-durability.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`

### Definition Of Done

- builder public opt-in path 获得 fresh runtime restart durability evidence
- 若 one-shot factory config 合同也落地，则该 public path 同样获得 fresh runtime durability evidence
- 若 fresh RED 没有指向 public config drift，本批保持 tests/plan/working-memory only
- capability wording、default shipped behavior、context/connection wiring 保持不变
- focused suite、adjacent regressions、completeness gate、compile gate、diff hygiene 都有 fresh evidence
