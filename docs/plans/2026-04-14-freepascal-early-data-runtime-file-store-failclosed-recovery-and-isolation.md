# FreePascal Early-Data Runtime File-Store Fail-Closed Recovery And Isolation Implementation Plan

**Goal:** 在不改变默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，把 file-backed anti-replay 已有 provider-level main/orphan/corrupt semantics 再提升到真实 runtime early-data `Accept` path，并补一条最小 store-boundary isolation/path-swap contract。

**Architecture:** 继续复用现有 focused scripted early-data runtime harness、backend-private installer seam、`BuildReplayProviderStoreFilePath(...)`、`CleanupReplayProviderStoreFiles(...)`、`WriteReplayProviderStore*` helpers、以及已存在的 restart / lock / mixed-path contracts。新工作优先落在 `tests/test_freepascal_tls13_early_data.pas`：把 corrupt main store、corrupt orphan temp store、orphan temp recovery、orphan lock ignore、以及 different store-file boundary isolation 提升到真实 resumed early-data `Accept` 路径。只有 fresh RED 明确指向 provider/runtime drift，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 或 `src/fafafa.ssl.freepascal.earlydatareplay.pas`。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 scripted resumed early-data runtime tests, backend-private `InstallFileBackedReplayLedger(...)`, file-backed replay-store binary helpers, file-based working memory.

### Files
- Create: `docs/plans/2026-04-14-freepascal-early-data-runtime-file-store-failclosed-recovery-and-isolation.md`
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

### Task 1: Plan And Working Memory

**Files:**
- Create: `docs/plans/2026-04-14-freepascal-early-data-runtime-file-store-failclosed-recovery-and-isolation.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Steps:**
1. 写本轮 plan，并把 `task_plan.md` / `findings.md` / `progress.md` 顶部切到 runtime file-store fail-closed/recovery/isolation 批次。
2. 明确本批优先保持 tests/harness only；默认不碰 `src/`。
3. 记录 focused / adjacent / gate / hygiene closeout 命令位与退出条件。

### Task 2: RED - Lift File-Store Semantics To Real Runtime Accept Path

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Add runtime corrupt-store fail-closed contract**
- 复用现有：
  - `WriteReplayProviderStoreHeader(...)`
  - `WriteReplayProviderTruncatedStoreFile(...)`
  - `BuildReplayProviderStoreFilePath(...)`
  - `CleanupReplayProviderStoreFiles(...)`
  - `CaptureServerIssuedSession(...)`
  - scripted resumed early-data runtime harness
- 新增 focused runtime contract，覆盖：
  - corrupt main replay-store file
  - resumed handshake 仍成功
  - session 仍 reused
  - early-data 被 reject
  - discarded bytes 不可读

**Step 2: Add runtime corrupt orphan-temp fail-closed contract**
- 复用现有 orphan `.tmp` setup helpers。
- 新增 focused runtime contract，覆盖：
  - canonical main file 缺失
  - corrupt orphan `.tmp` store 存在
  - real resumed early-data `Accept` path 继续 fail closed

**Step 3: Add runtime orphan recovery / ignore contracts**
- 新增 focused runtime contracts，覆盖：
  - orphan `.tmp` store 含 live replay truth 时，real runtime path 仍 reject replay
  - orphan `.lock` file 无 active holder 时，fresh acquire 不应被误阻断，runtime path 仍 accept

**Step 4: Add runtime store-boundary isolation contract**
- 新增 focused runtime contract，覆盖：
  - same resumable session 在 store file A 上 first accept
  - 切到 store file B 后，不应静默继承 file A 的 replay truth
  - different file boundary 应形成独立 truth

**Step 5: Add minimal harness cleanup if it directly supports these contracts**
- 只允许最小 tests-only cleanup，例如：
  - 提取 child `context_path` normalization helper，避免 duplicated logic
- 不做 unrelated refactor

**Step 6: Verify RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - first fresh failure should come from missing runtime contract handling or a real provider/runtime drift

### Task 3: GREEN - Keep The Fix Surface Minimal

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Steps:**
1. 优先只改 tests/harness，只有 fresh RED 明确说明 existing runtime helper 形状不足时才做最小 helper 补强。
2. 若 fresh RED 明确落到 file-backed provider/load/save/lock/orphan runtime drift，再最小查看：
   - `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
   - `src/fafafa.ssl.freepascal.earlydatareplay.pas`
3. 不先碰：
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
   - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_runtime_file_store_failclosed_recovery_isolation_20260414`
4. 跑 compile gate：
   - `python3 scripts/compile_all_modules.py`
5. 跑 limited git hygiene + direct file scan：
   - `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-runtime-file-store-failclosed-recovery-and-isolation.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
   - 直接扫描上述文件的 trailing whitespace / final newline
6. 用 fresh evidence 更新 roadmap 与 working memory，并把本批状态收口为 completed。

### Outcome
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增 5 条 installer runtime contracts 与最小 runtime helpers，把 file-backed provider 已经成立的 main/orphan/lock/path-swap 语义直接抬到真实 resumed early-data `Accept` path。
- fresh focused run 直接 GREEN，没有出现任何 fresh RED 指向 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`、`src/fafafa.ssl.freepascal.earlydatareplay.pas`、`src/fafafa.ssl.factory.pas`、`src/fafafa.ssl.context.builder.pas`、`src/fafafa.ssl.freepascal.context.pas` 或 `src/fafafa.ssl.freepascal.connection.pas`；因此本批继续保持 tests/docs/working-memory only。
- capability wording 保持不变：`0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_runtime_file_store_failclosed_recovery_isolation_20260414`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-runtime-file-store-failclosed-recovery-and-isolation.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
  - PASS：无输出
- direct whitespace / newline scan
  - `docs/plans/2026-04-14-freepascal-early-data-runtime-file-store-failclosed-recovery-and-isolation.md`、`docs/ROADMAP.md`、`tests/test_freepascal_tls13_early_data.pas`、`task_plan.md`、`progress.md` 无 trailing whitespace，且保留 final newline
  - `findings.md` 顶部本批 writeback 无问题；文件更深处仍有 pre-existing trailing whitespace 历史噪音，本批未顺手重写
