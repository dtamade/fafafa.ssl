# FreePascal Early-Data Directory-Store Runtime Crash-Update Restart Implementation Plan

**Goal:** 在不改默认 shipped behavior、public API、builder / factory / config surface、capability wording、`TFreePascalContext` 或 `TFreePascalConnection` wiring 的前提下，把 directory-store 更接近真实 accepted update-path 的 runtime crash-window 语义锁成 focused contract：existing replay truth 先已存在，fresh resumed early-data 在新进程里被 accept 并更新 anti-replay state，随后进程在 accept 后立刻 crash；重启后，既有 replay truth 与刚 accept 的 replay truth 都必须继续 reject。

**Architecture:** 继续严格 tests-first。优先只扩 `tests/test_freepascal_tls13_early_data.pas` 的现有 runtime child harness：让 `TEST_REPLAY_PROVIDER_RUNTIME_CRASH_ACCEPT_MODE` 支持 `directory_store` context path，并把 canonical store-state assertions 泛化到 file-backed / directory-backed 两种 shape。然后在同一 focused 文件里新增一条 runtime contract，复用 `BuildDirectoryReplayStoreServerContext(...)`、`AssertResumedEarlyDataAcceptedAtRuntime(...)`、`AssertResumedEarlyDataRejectedAtRuntime(...)`、`CaptureServerIssuedSession(...)`、`RunReplayProviderRuntimeReplayProbeMode(...)` 现有 child mode。只有 fresh RED 明确落到 `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas` 时，才最小修法。

**Tech Stack:** FreePascal (ObjFPC), directory-backed local replay store, TLS 1.3 early-data focused runtime tests, backend-private store-backed replay seam, completeness gate, `python3 scripts/compile_all_modules.py`, file-based working memory.

---

## Summary

1. 先扩 runtime crash-accept child harness，让它支持 `directory_store` context path。
2. 再补一条 directory-store runtime crash-window update contract：
   - parent 先 materialize existing replay truth
   - child accept fresh blocked session 后 simulated crash
   - restart 后 blocked session reject，original replay truth 也 reject
3. 跑 focused suite，看 fresh RED 是 tests-harness seam 还是 production drift。
4. 只有需要时最小碰 `src/`；随后跑 adjacent / gate / compile / diff hygiene，并回写 roadmap / working-memory。

## Task 1: RED - Extend Runtime Crash Harness For Directory Store

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Steps:**
- 让 `RunReplayProviderRuntimeCrashAcceptMode(...)` 支持一个可选 context path 参数。
- 让 `HandleReplayProviderChildMode` 在 `--runtime-crash-accept` 模式下把可选的第 6 个参数传给 child helper。
- crash helper 对落盘结果不再只断言 file-backed main file，而是复用现有 store-state helper 去断言 canonical replay truth 已 materialize。

## Task 2: RED - Lock Directory-Store Crash-Update Restart Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Steps:**
- 新增一条 runtime focused contract，覆盖：
  - parent 先 accept existing session，materialize canonical directory replay truth
  - child 进程对 blocked session accept 后 simulated crash
  - crash 后 canonical directory replay truth 仍存在
  - new runtime context 继续 reject original existing session
  - replay-probe child 继续 reject blocked session，并记录 `directory_store` context path

## Task 3: GREEN - Keep Production Touches Conditional

**Files:**
- Modify only if needed: `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas`

**Steps:**
- 若 focused suite 在 tests-harness 修完后直接 GREEN，则保持 tests-only closeout。
- 若 fresh RED 明确落到 `dirstore` load/save / restart drift，再做最小生产修法，不重开 public wiring 或其他 provider family。

## Task 4: Verify And Close Out

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Verification:**
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
- `export PATH="/opt/fpcupdeluxe/fpc/bin/x86_64-linux:$PATH" && bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_directory_store_runtime_crash_update_restart_20260416`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-04-16-freepascal-early-data-directory-store-runtime-crash-update-restart.md tests/test_freepascal_tls13_early_data.pas src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas docs/ROADMAP.md task_plan.md findings.md progress.md`

### Definition Of Done

- runtime crash-accept child harness supports `directory_store` context path
- directory-store accepted update-path crash-window restart is covered by a focused runtime contract
- restart after simulated crash still rejects both original replay truth and the just-accepted blocked session
- focused / adjacent / completeness / compile / diff hygiene all have fresh evidence
- capability wording remains `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
