# FreePascal Early-Data Store-Path Identity And Cross-Process Boundary Implementation Plan

**Goal:** 在不改变默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 FreePascal early-data file-backed anti-replay 再补一批最小但高 ROI 的 durability 合同：锁住“同一物理 replay-store file 的不同路径表示仍共享同一 replay truth boundary”，以及“不同 replay-store file 在跨进程场景下继续形成独立 truth boundary”。

**Architecture:** 继续复用现有 focused scripted early-data runtime harness、`BuildReplayProviderStoreFilePath(...)`、`BuildReplayProviderMarkerFilePath(...)`、`CleanupReplayProviderStoreFiles(...)`、`CaptureServerIssuedSession(...)`、`AssertResumedEarlyDataAcceptedAtRuntime(...)`、`AssertResumedEarlyDataRejectedAtRuntime(...)`、child self-exec `TEST_REPLAY_PROVIDER_RUNTIME_REPLAY_MODE`、以及现成的 `TEST_REPLAY_PROVIDER_RUNTIME_CRASH_ACCEPT_MODE`。优先把工作收敛在 `tests/test_freepascal_tls13_early_data.pas`：先补 relative/absolute same-file identity contract，再补 cross-process different-store boundary contract。只有 fresh RED 明确指出真实 provider/runtime drift，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 或 `src/fafafa.ssl.freepascal.earlydatareplay.pas`。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 scripted resumed early-data runtime tests, backend-private `InstallFileBackedReplayLedger(...)`, file-backed replay-store binary helpers, child-process self-exec, file-based working memory.

### Files
- Create: `docs/plans/2026-04-15-freepascal-early-data-store-path-identity-and-cross-process-boundary.md`
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

### Task 1: Plan And Working Memory

**Files:**
- Create: `docs/plans/2026-04-15-freepascal-early-data-store-path-identity-and-cross-process-boundary.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Steps:**
1. 写本轮 plan，并把 `task_plan.md` / `findings.md` / `progress.md` 顶部切到 store-path identity / cross-process boundary 批次。
2. 记录当前判断：默认优先保持 tests/harness only，不重开 seam / builder / factory / context / connection wiring。
3. 记录 focused / adjacent / gate / hygiene closeout 命令位与退出条件。

### Task 2: RED - Lock Same-File Path Identity Convergence

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Add same-process relative/absolute same-file contract**
- 复用现有：
  - `BuildReplayProviderStoreFilePath(...)`
  - `BuildAcceptingEarlyDataServerContext(...)`
  - `AssertResumedEarlyDataAcceptedAtRuntime(...)`
  - `AssertResumedEarlyDataRejectedAtRuntime(...)`
- 新增 focused runtime contract，覆盖：
  - 同一个 replay-store file 先用 relative path 安装并 accept first resumed early-data
  - 再用 `ExpandFileName(...)` 得到 absolute alias
  - 用 absolute alias 重建/重装后，同一 session 继续 replay reject
  - 证明 replay truth boundary 绑定的是物理 store，而不是未经锁定的原始路径字符串

**Step 2: Add cross-process same-file alias contract**
- 继续复用：
  - `TEST_REPLAY_PROVIDER_RUNTIME_REPLAY_MODE`
  - `RunReplayProviderRuntimeReplayProbeMode(...)`
  - `BuildReplayProviderMarkerFilePath(...)`
- 新增 focused runtime contract，覆盖：
  - parent 进程用 relative path materialize replay truth
  - child 进程对同一物理 file 使用 absolute alias
  - child 仍 reject replay，且 fresh resumed early-data 继续 accept

**Step 3: Verify RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - first fresh failure should come from missing contract wiring or a real path-identity drift

### Task 3: RED - Lock Cross-Process Different-Store Boundary Isolation

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Reuse existing crash-accept and replay-probe child modes**
- 优先复用：
  - `TEST_REPLAY_PROVIDER_RUNTIME_CRASH_ACCEPT_MODE`
  - `TEST_REPLAY_PROVIDER_RUNTIME_REPLAY_MODE`
  - `WaitForFileExists(...)`
  - ready / graceful / session markers
- 默认不新增 child mode，除非 fresh RED 明确说明现有 helper 无法稳定表达 boundary truth

**Step 2: Add cross-process different-store boundary contract**
- 新增 focused runtime contract，覆盖：
  - parent 在 store file A 上 accept 并 materialize replay truth
  - child 在 different store file B 上对同一 session 仍可 first-accept
  - child 在 file B 上 materialize 后，后续 replay probe 继续 reject
  - 证明 A/B 继续是独立 replay truth boundary，而不是跨进程共享隐式内存或路径污染

**Step 3: Verify RED**
- Re-run Task 2 focused command
- Expected:
  - first fresh failure should come from missing boundary contract wiring or a real provider/runtime drift

### Task 4: GREEN - Keep The Fix Surface Minimal

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Steps:**
1. 优先只改 tests/harness；只有 fresh RED 明确说明现有 child mode / helper 形状不足时，才做最小 helper 补强。
2. 若 fresh RED 明确落到 file-backed provider 路径解析、load/save 或 lock/orphan 语义漂移，再最小查看：
   - `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
   - `src/fafafa.ssl.freepascal.earlydatareplay.pas`
3. 不先碰：
   - `src/fafafa.ssl.factory.pas`
   - `src/fafafa.ssl.context.builder.pas`
   - `src/fafafa.ssl.freepascal.context.pas`
   - `src/fafafa.ssl.freepascal.connection.pas`

### Task 5: Verification And Closeout

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
   - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_store_path_identity_cross_process_boundary_20260415`
4. 跑 compile gate：
   - `python3 scripts/compile_all_modules.py`
5. 跑 limited git hygiene + direct file scan：
   - `git diff --check -- docs/plans/2026-04-15-freepascal-early-data-store-path-identity-and-cross-process-boundary.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
   - 直接扫描上述文件的 trailing whitespace / final newline
6. 用 fresh evidence 更新 roadmap 与 working memory，并把本批状态收口为 completed。

### Outcome
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 3 条 focused runtime contracts，分别锁住：
  - installer runtime path 的 relative/absolute same-file identity convergence
  - parent relative -> child absolute 的 same-file cross-process convergence
  - parent file A -> child file B 的 cross-process different-store boundary isolation
- fresh RED 只落在 focused child replay probe 的硬编码预期，而不是 production drift：
  - 原 helper 默认要求 parent 已 materialize 同一个 store file
  - 原 helper 默认第一条 resumed attempt 必须 `reject`
- 最小 GREEN 也因此只需要 tests/harness 补强：
  - `TEST_REPLAY_PROVIDER_EXPECT_REJECT`
  - `TEST_REPLAY_PROVIDER_EXPECT_ACCEPT_THEN_REJECT`
  - `RunReplayProviderRuntimeReplayProbeMode(...)` 新增可选 expectation 分支
  - `HandleReplayProviderChildMode` 解析第 6 个可选 expectation 参数
- 没有修改任何 `src/` 单元；capability wording 保持不变：
  - `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - first fresh run: FAIL（`Runtime replay probe helper should observe persisted replay-store state from the parent process`）
  - after minimal harness fix: PASS（`✅ FreePascal TLS 1.3 early-data checks passed`）
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_store_path_identity_cross_process_boundary_20260415`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-15-freepascal-early-data-store-path-identity-and-cross-process-boundary.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
  - PASS：无输出
- direct whitespace / newline scan
  - `docs/plans/2026-04-15-freepascal-early-data-store-path-identity-and-cross-process-boundary.md`、`docs/ROADMAP.md`、`tests/test_freepascal_tls13_early_data.pas`、`task_plan.md`、`progress.md` 无 trailing whitespace，且保留 final newline
  - `findings.md` 本批顶部 writeback 与 final newline 正常；文件更深处的历史空白噪音本批未做全量重扫或重写
