# FreePascal Early-Data Mixed Public-Path Cross-Process Durability Implementation Plan

**Goal:** 在不改变默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 FreePascal early-data file-backed anti-replay 再补一层最高 ROI 的 cross-process mixed public-path runtime evidence：父进程经 builder 或 one-shot factory materialize 的 replay truth，在子进程经另一条 public path 重建后仍会 reject replay，同时 fresh resumed early-data 继续 accept。

**Architecture:** 继续复用现有 focused runtime restart primitives：`BuildReplayProviderStoreFilePath(...)`、`BuildReplayProviderMarkerFilePath(...)`、`CleanupReplayProviderStoreFiles(...)`、`WriteBytesToFile(...)`、`CaptureServerIssuedSession(...)`、以及现有 child self-exec `TEST_REPLAY_PROVIDER_RUNTIME_REPLAY_MODE`。新的 contracts 只落在 `tests/test_freepascal_tls13_early_data.pas`：父进程继续分别走 builder 与 one-shot factory public path；child replay probe 只做最小可替换扩展，新增一个 optional public-path selector，并在 child 内部经 builder 或 one-shot factory 重建 server context。只有 fresh RED 明确指出真实实现 drift，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 或 `src/fafafa.ssl.freepascal.earlydatareplay.pas`。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data scripted runtime tests, `TSSLContextBuilder`, `TSSLFactory.CreateContext(const AConfig)`, file-backed replay-store opt-in, child-process self-exec, file-based working memory.

### Files
- Create: `docs/plans/2026-04-14-freepascal-early-data-mixed-public-path-cross-process-durability.md`
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

### Task 1: Plan And Working Memory

**Files:**
- Create: `docs/plans/2026-04-14-freepascal-early-data-mixed-public-path-cross-process-durability.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Steps:**
1. 写本轮 plan，并把 `task_plan.md` / `findings.md` / `progress.md` 顶部切到 mixed public-path cross-process durability 批次。
2. 明确本批只优先动 focused tests / harness；默认不碰 `src/`。
3. 记录 verification closeout 命令位与退出条件。

### Task 2: RED - Add Mixed Public-Path Cross-Process Runtime Contracts

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Extend child replay probe selector contract**
- 继续复用 `TEST_REPLAY_PROVIDER_RUNTIME_REPLAY_MODE`，不新增 child mode。
- 给 replay probe 增加 optional public-path selector：
  - default / empty => 继续走现有 installer-based path
  - `builder` => child 通过 builder public path 创建 context
  - `factory` => child 通过 one-shot factory public path 创建 context
- 加一个最小 marker 证据，确保 tests 能证明 child 确实走到了请求的 public construction path，而不是静默回退到 installer path。

**Step 2: Add builder-parent / factory-child restart contract**
- 新增 focused runtime contract，覆盖：
  - builder-built parent context 指向 file-backed replay-store file
  - parent first resumed early-data accept，并落盘 serialized session 与 replay truth
  - child replay probe 指定 `factory` public path 重建 context
  - child 仍 reject replay，且 fresh resumed early-data 继续 accept

**Step 3: Add factory-parent / builder-child restart contract**
- 新增镜像 focused runtime contract，覆盖：
  - one-shot factory-built parent context 指向同一类 replay-store file
  - parent first resumed early-data accept，并落盘 serialized session 与 replay truth
  - child replay probe 指定 `builder` public path 重建 context
  - child 仍 reject replay，且 fresh resumed early-data 继续 accept

**Step 4: Verify RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - first fresh failure should come from missing public-path selector / marker support, or a real mixed-path replay drift
  - if suite already passes, do not force a `src/` edit

### Task 3: GREEN - Minimal Harness Change, Then Only If Needed Production Change

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Keep default replay probe behavior intact**
- 只做最小 replay-probe helper 扩展：
  - optional selector
  - optional marker
  - default installer path 行为保持不变，避免影响现有 restart / crash / lock contracts

**Step 2: Only inspect `src/` if fresh RED proves real implementation drift**
- 如果 child builder/factory public path 明确不能消费 parent materialized replay truth，再最小查看：
  - `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
  - `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- 不先碰：
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.freepascal.context.pas`
  - `src/fafafa.ssl.freepascal.connection.pas`

### Task 4: Verification And Closeout

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Steps:**
1. 跑 focused suite。
2. 跑 adjacent regressions：
   - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
   - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
3. 跑 completeness gate：
   - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_mixed_public_path_cross_process_durability_20260414`
4. 跑 compile gate：
   - `python3 scripts/compile_all_modules.py`
5. 跑 diff hygiene：
   - `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-mixed-public-path-cross-process-durability.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
6. 用 fresh evidence 更新 roadmap 与 working memory，并把本批状态收口为 completed。
