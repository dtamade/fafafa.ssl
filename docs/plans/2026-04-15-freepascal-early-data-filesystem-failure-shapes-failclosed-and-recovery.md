# FreePascal Early-Data Filesystem Failure Shapes Fail-Closed And Recovery Implementation Plan

**Goal:** 在不改默认 shipped behavior、capability wording、`TFreePascalContext` / `TFreePascalConnection` wiring 的前提下，为 file-backed replay-store 再补一批高 ROI 的 filesystem failure-shape 合同：锁住路径阻塞时 fail-closed，锁住 blocker 移除后 recovery 不会消费 replay truth，也不会阻止后续 canonical main store materialization。

**Architecture:** 继续优先落在 `tests/test_freepascal_tls13_early_data.pas`。direct provider 合同与 installer runtime 合同都只复用已有 file-backed helper、runtime assertion helper 与 focused suite register path；只有 fresh RED 明确落到 provider 实现漂移，才值得最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。fresh evidence 最终证明当前 shipped 语义已经天然满足这批合同，因此本批保持 tests/docs/working-memory only。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 scripted resumed early-data runtime tests, file-backed replay-store provider, backend-private installer seam, file-based working memory.

### Files
- Create: `docs/plans/2026-04-15-freepascal-early-data-filesystem-failure-shapes-failclosed-and-recovery.md`
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

### Task 1: Plan And Working Memory

**Files:**
- Create: `docs/plans/2026-04-15-freepascal-early-data-filesystem-failure-shapes-failclosed-and-recovery.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Steps:**
1. 重读 roadmap / working-memory / focused helper / file-backed provider，确认 current highest ROI 是 filesystem failure shapes，而不是重开 seam、builder/factory parity 或 context/connection wiring。
2. 记录本批最小目标：
   - `<store>.lock` 被目录占位
   - `<store>.tmp` 被目录占位
   - store parent path 被普通文件占位导致 `ForceDirectories(...)` 失败
3. 记录默认实现策略：先补 focused contracts；若 fresh focused run 直接 GREEN，则不修改 `src/`。

### Task 2: RED - Direct Provider Filesystem Failure Shapes

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Steps:**
1. 新增 direct provider contract，锁住 `<store>.lock` 被目录占位时 fail closed。
2. 在同一条 contract 中继续锁住 recovery：
   - blocker 移除后，同一 session first acquire 重新成功
   - canonical main store file materialize
   - provider rebuild 后对同一 session 继续 reject replay
3. 再补 `<store>.tmp` 目录占位与 parent path file-blocker 两条同形 contract。
4. 运行 focused suite，看 fresh failure 是否出现。

### Task 3: RED - Installer Runtime Filesystem Failure Shapes

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Steps:**
1. 复用 `BuildInstallerFileBackedReplayStoreServerContext(...)`、`AssertResumedEarlyDataAcceptedAtRuntime(...)`、`AssertResumedEarlyDataRejectedAtRuntime(...)`。
2. 新增 runtime contract，分别锁住：
   - `.lock` directory blocker
   - `.tmp` directory blocker
   - parent path file blocker
3. 每个 runtime 子场景都要求：
   - blocker 存在时 resumed handshake success、session reused、early-data rejected、discarded bytes 不泄漏
   - blocker 移除后同一 session 可 accept
   - recovery 之后同一 session 再次 reject，证明 replay truth 正常 materialize
4. 运行 focused suite，确认是否需要任何 `src/` 修复。

### Task 4: GREEN - Keep The Fix Surface Minimal

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Steps:**
1. 优先只改 tests/harness。
2. 如果需要 tests cleanup，仅允许做直接支撑这批 contract 的最小 helper 收口。
3. 只有 fresh RED 明确指向 provider drift，才最小查看 `OpenLockFileStream(...)`、`AcquireUpdateGuard(...)`、`SaveEntries(...)`。

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
   - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_filesystem_failure_shapes_20260415`
4. 跑 compile gate：
   - `python3 scripts/compile_all_modules.py`
5. 跑 hygiene：
   - `git diff --check -- docs/plans/2026-04-15-freepascal-early-data-filesystem-failure-shapes-failclosed-and-recovery.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
6. 用 fresh evidence 更新 roadmap 与 working memory。

### Outcome
- 已完成。`tests/test_freepascal_tls13_early_data.pas` 新增两组 focused contracts：
  - `TestFileBackedReplayProviderFailsClosedOnFilesystemPathBlockersAndRecovers`
  - `TestContextFileBackedReplayInstallerFailsClosedOnFilesystemPathBlockersAtRuntime`
- 同时对 tests-only cleanup 做了一个最小收口：
  - `RemoveReplayProviderPathIfExists(...)`
  - `CleanupReplayProviderStoreFiles(...)` 现在可清理 file 或 empty-directory blocker
- 三类 filesystem failure shapes 已被 fresh evidence 锁住：
  - `<store>.lock` 被目录占位
  - `<store>.tmp` 被目录占位
  - parent path 被普通文件占位导致 `ForceDirectories(...)` 失败
- fresh focused run 直接 GREEN，说明当前 file-backed provider/runtime shipped 语义已经天然满足 fail-closed + recovery 合同；本批没有修改任何 `src/` 单元。

### Verification
- `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - PASS：`✅ FreePascal TLS 1.3 early-data checks passed`
- `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - PASS：`✅ FreePascal backend basic checks passed`
- `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - PASS：`✓ FreePascal KnownIssues runtime alignment verified`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_filesystem_failure_shapes_20260415`
  - PASS：`[PASS] freepascal tls13 completeness gate finished`
- `python3 scripts/compile_all_modules.py`
  - PASS：`编译成功: 184 (100.0%)`
- `git diff --check -- docs/plans/2026-04-15-freepascal-early-data-filesystem-failure-shapes-failclosed-and-recovery.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
  - PASS：无输出
- `rg -n "[ \t]+$" docs/plans/2026-04-15-freepascal-early-data-filesystem-failure-shapes-failclosed-and-recovery.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md progress.md`
  - PASS：无输出
- final newline check
  - `docs/plans/2026-04-15-freepascal-early-data-filesystem-failure-shapes-failclosed-and-recovery.md`、`docs/ROADMAP.md`、`tests/test_freepascal_tls13_early_data.pas`、`task_plan.md`、`progress.md` 均保留 final newline
  - `findings.md` 顶部本批 writeback 正常；文件更深处仍有 pre-existing trailing whitespace 历史噪音，本批未做全量重写
