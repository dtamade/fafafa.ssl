# FreePascal Early-Data Installer-To-Public Mixed Cross-Process Convergence Implementation Plan

**Goal:** 在不改变默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 FreePascal early-data file-backed anti-replay 再补一层最高 ROI 的 mixed cross-process convergence evidence：backend-private installer 父进程 materialize 的 replay truth，在子进程经 builder 或 one-shot factory public path 重建后仍会 reject replay，同时 fresh resumed early-data 继续 accept。

**Architecture:** 继续复用现有 focused runtime restart primitives：`BuildReplayProviderStoreFilePath(...)`、`BuildReplayProviderMarkerFilePath(...)`、`CleanupReplayProviderStoreFiles(...)`、`WriteBytesToFile(...)`、`CaptureServerIssuedSession(...)`、以及已经扩好的 child self-exec `TEST_REPLAY_PROVIDER_RUNTIME_REPLAY_MODE`。新的 contracts 只落在 `tests/test_freepascal_tls13_early_data.pas`：父进程继续走 installer seam；child replay probe 只复用现有 optional public-path selector 与 `context_path` marker，在 child 内部经 builder 或 one-shot factory 重建 server context。只有 fresh RED 明确指出真实实现 drift，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 或 `src/fafafa.ssl.freepascal.earlydatareplay.pas`。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data scripted runtime tests, backend-private `InstallFileBackedReplayLedger(...)`, `TSSLContextBuilder`, `TSSLFactory.CreateContext(const AConfig)`, file-backed replay-store opt-in, child-process self-exec, file-based working memory.

### Files
- Create: `docs/plans/2026-04-14-freepascal-early-data-installer-public-mixed-cross-process-convergence.md`
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

### Task 1: Plan And Working Memory

**Files:**
- Create: `docs/plans/2026-04-14-freepascal-early-data-installer-public-mixed-cross-process-convergence.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Steps:**
1. 写本轮 plan，并把 `task_plan.md` / `findings.md` / `progress.md` 顶部切到 installer-to-public mixed cross-process convergence 批次。
2. 明确本批优先保持 tests/harness only；默认不碰 `src/`。
3. 记录 verification closeout 命令位与退出条件。

### Task 2: RED - Add Installer-To-Public Mixed Cross-Process Runtime Contracts

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Reuse installer-parent restart setup**
- 复用现有 installer 父路径：
  - `TSSLFactory.CreateContext(sslCtxServer, sslFreePascal)`
  - `PrepareServerContextForEarlyData(...)`
  - `IFreePascalContextEarlyDataReplayInstaller.InstallFileBackedReplayLedger(...)`
  - `CaptureServerIssuedSession(...)`
- 复用 child replay mode：
  - `TEST_REPLAY_PROVIDER_RUNTIME_REPLAY_MODE`
  - `RunReplayProviderRuntimeReplayProbeMode(...)`
  - `context_path` marker

**Step 2: Add installer-parent / builder-child restart contract**
- 新增 focused runtime contract，覆盖：
  - installer-built parent context 指向 file-backed replay-store file
  - parent first resumed early-data accept，并落盘 serialized session 与 replay truth
  - child replay probe 指定 `builder` public path 重建 context
  - child 仍 reject replay，且 fresh resumed early-data 继续 accept
  - marker 证明 child 确实走了 builder public path

**Step 3: Add installer-parent / factory-child restart contract**
- 新增镜像 focused runtime contract，覆盖：
  - installer-built parent context 指向 file-backed replay-store file
  - parent first resumed early-data accept，并落盘 serialized session 与 replay truth
  - child replay probe 指定 `factory` public path 重建 context
  - child 仍 reject replay，且 fresh resumed early-data 继续 accept
  - marker 证明 child 确实走了 factory public path

**Step 4: Verify RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - first fresh failure should come from missing test wiring or a real installer->public convergence drift
  - if suite already passes, do not force a `src/` edit

### Task 3: GREEN - Only If Needed, Keep Harness Changes Minimal

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Reuse existing selector/marker without widening protocol**
- 不新增 child mode
- 不新增额外 sidecar protocol
- 直接复用现有 selector / marker

**Step 2: Only inspect `src/` if fresh RED proves real implementation drift**
- 若 installer-parent materialized truth 无法被 builder/factory child 消费，再最小查看：
  - `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
  - `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- 不先碰：
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.context.builder.pas`
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
   - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_installer_public_mixed_cross_process_convergence_20260414`
4. 跑 compile gate：
   - `python3 scripts/compile_all_modules.py`
5. 跑 limited git hygiene + direct file scan：
   - `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-installer-public-mixed-cross-process-convergence.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
   - 直接扫描上述文件的 trailing whitespace / final newline
6. 用 fresh evidence 更新 roadmap 与 working memory，并把本批状态收口为 completed。

## Outcome

- 实际只需要最小 tests/harness 收口：
  - `tests/test_freepascal_tls13_early_data.pas` 新增 installer-parent -> builder-child / factory-child 两条 runtime contracts
  - child `context_path` marker 从“只建文件”收紧为“写入 normalized selected public path”
- fresh RED 没有落到任何生产实现 drift；没有修改：
  - `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
  - `src/fafafa.ssl.freepascal.earlydatareplay.pas`
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.context.builder.pas`
  - `src/fafafa.ssl.freepascal.context.pas`
  - `src/fafafa.ssl.freepascal.connection.pas`
- 最终 fresh evidence 证明：
  - backend-private installer 父路径 materialized 的 persisted replay truth
  - 在 child 走 builder 或 one-shot factory public path 重建后
  - 仍会 reject replay，同时 fresh resumed early-data 继续 accept

## Verification

- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - first fresh run: FAIL（`❌ Installer-parent/builder-child runtime replay probe should record the requested builder public path`）
  - after minimal harness fix: PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_installer_public_mixed_cross_process_convergence_20260414`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-installer-public-mixed-cross-process-convergence.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
  - PASS：无输出

## Final Notes

- capability wording 保持不变：
  - `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
- 本批 next queue 仍然是更重 provider / durability 形态验证，而不是重开当前 seam / public wiring。
