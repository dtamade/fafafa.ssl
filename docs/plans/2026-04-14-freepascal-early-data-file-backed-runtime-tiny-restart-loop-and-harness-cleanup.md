# FreePascal Early-Data File-Backed Runtime Tiny Restart Loop And Harness Cleanup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改 public API、builder/factory/config surface、`TFreePascalContext` / `TFreePascalConnection` wiring 与 capability wording 的前提下，为 FreePascal early-data file-backed anti-replay opt-in 路径补一个最小 3-round runtime restart smoke，并顺手收敛最小测试 harness cleanup，继续以最低返工成本锁住 restart durability truth。

**Architecture:** 继续复用现有 backend-private file-backed installer seam、`RunReplayProviderRuntimeReplayProbeMode(...)` child self-exec helper、`TFreePascalSession.Serialize/Deserialize`、以及现有 focused runtime restart contract。第一优先级仍是 tests-first：先在 `tests/test_freepascal_tls13_early_data.pas` 加一个 tiny loop 合同；若 fresh evidence 显示只是 harness 冗余，就只做 tests-only helper cleanup；只有 fresh RED 明确指出真实实现缺口时，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data scripted runtime tests, backend-private replay installer seam, child-process self-exec, session serialization, file-based working memory.

---

## Summary

- 当前 P0 runtime truth 已经有：
  - process restart durability
  - crash-window restart durability
  - cross-process lock-contention fail-closed
- 当前最高 ROI 的下一批不是继续扩生产面，而是：
  - 用一个极小的 3-round restart smoke 防止 repeated restart / probe 下 replay truth 漂移
  - 把现在零散的 sidecar cleanup 收敛成更稳的 focused helper，减少后续 test-only 返工
- 本批明确不做：
  - distributed / multi-host persistence
  - capability wording 升级
  - public API / builder / factory / config 扩面
  - `context.pas` / `connection.pas` 结构调整

## Delivery Order

1. 写本轮 plan，并把 `task_plan.md` / `findings.md` / `progress.md` 顶部切到 tiny restart loop 批次。
2. 拉起 `gpt-5.4` 团队：一个 worker 只负责 `tests/test_freepascal_tls13_early_data.pas`，一个 reviewer 只看 harness brittleness / cleanup。
3. 在 `tests/test_freepascal_tls13_early_data.pas` 先补 tiny 3-round restart loop runtime contract。
4. 只在需要时补最小 helper cleanup；避免 unrelated refactor。
5. 跑 focused suite；只有 fresh RED 明确指向真实实现缺口时，才最小看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
6. 跑 adjacent regressions、completeness gate、compile gate、diff hygiene。
7. 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Task 1: RED - Lock A Tiny 3-Round Restart Loop

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Reuse existing runtime restart primitives**
- 复用现有：
  - `BuildReplayProviderStoreFilePath(...)`
  - `BuildReplayProviderMarkerFilePath(...)`
  - `CleanupReplayProviderStoreFiles(...)`
  - `DeleteFileIfExists(...)`
  - `WriteBytesToFile(...)`
  - `RunReplayProviderRuntimeReplayProbeMode(...)`
  - `CaptureServerIssuedSession(...)`

**Step 2: Add a tiny restart smoke contract**
- 新增 focused runtime contract，覆盖：
  - 仅做 3 轮
  - 每轮使用独立 replay-store 文件名
  - 每轮都验证：
    - parent first resumed early-data accept 仍成功
    - child replay probe 在新进程里仍 reject 同一 session
    - child fresh resumed session 仍 accept
  - 不引入 sleep / 随机化 / 额外 child protocol

**Step 3: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - 若 repeated restart / probe truth 有漂移，应优先在新 tiny loop 合同上 RED
  - 若现有实现已天然满足，则直接 GREEN，随后只做 test-harness cleanup

### Task 2: GREEN / REFACTOR - Minimal Harness Cleanup Only

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Tighten cleanup scope**
- 只做最小 cleanup：
  - 让 `CleanupReplayProviderStoreFiles(...)` 一并清掉 runtime test 会创建的稳定 sidecar（例如 `session.bin`、`graceful`）
  - 或提取一个更小的 test-only helper，避免多处重复 `DeleteFileIfExists(...)`
- 不改 child protocol，不改生产单位，不做 unrelated helper 重排

**Step 2: Keep suite registration explicit**
- 确保新的 tiny loop 合同被加到 run list，且位置紧邻现有 runtime restart / crash / lock contracts，保持 focused suite 语义连续

### Task 3: Verify Adjacent Truth Still Holds

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
- Reference: `tests/test_freepascal_backend_basic.pas`
- Reference: `tests/test_capability_cache.pas`

**Step 1: Re-run focused and adjacent verification**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_file_backed_runtime_tiny_restart_loop_20260414`
  - `python3 scripts/compile_all_modules.py`

**Step 2: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-file-backed-runtime-tiny-restart-loop-and-harness-cleanup.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`

### Definition Of Done

- file-backed installer/runtime path 额外获得 fresh tiny repeated-restart smoke evidence
- 若 helper cleanup 落地，则只限 tests-only harness 收敛，不触碰生产代码
- capability wording、public surface、context/connection wiring 保持不变
- focused suite、adjacent regressions、completeness gate、compile gate、diff hygiene 都有 fresh evidence
