# FreePascal Early-Data Runtime Parity And Doc Closeout Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改 public API、builder/factory/config surface、`TFreePascalContext` / `TFreePascalConnection` wiring 与 capability wording 的前提下，为 FreePascal early-data callback/file-backed opt-in 路径补一层真实 runtime parity evidence，并把当前 shipped truth / opt-in 边界回写到用户文档。

**Architecture:** 继续复用现有 callback helper、file-backed installer seam、builder/config opt-in 与 resumed early-data accept path，不新增任何生产 abstraction。第一优先级是在 `tests/test_freepascal_tls13_early_data.pas` 上补 runtime contracts，锁住“local lifecycle toggle 不会 wipe callback/file-backed replay truth，但恢复后 fresh resumed path 仍按 replay truth reject”的真实握手行为；如果 focused runtime contracts 直接 GREEN，则生产代码保持不动，只更新 `docs/ROADMAP.md` 与 `docs/INTEGRATION_GUIDE.md`，把默认 in-memory single-process truth、file-backed opt-in truth 与 managed-seam boundary 说清楚。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data scripted runtime tests, backend-private installer/helper seams, roadmap/integration docs, completeness gate, file-based working memory.

---

## Summary

- 当前 live truth：
  - default in-memory replay ledger 已收敛到底层 replay-store seam
  - managed seam 边界已经锁住：shared in-memory only；callback/file-backed non-managed providers 不被 local lifecycle toggle 隐式 wipe
  - callback helper / file-backed installer / builder opt-in 的 direct contracts 都已稳定
- 当前最高 ROI 的下一步不是再改生产行为，而是：
  - 把这些边界 through real resumed early-data runtime path 再锁一层
  - 把 roadmap / integration docs 更新到最新 shipped truth
- 本批明确不做：
  - capability wording 升级
  - public surface 扩面
  - distributed / default durable anti-replay
  - `context.pas` / `connection.pas` 结构调整

## Delivery Order

1. 写本轮 plan 与 working-memory 入口。
2. 在 `tests/test_freepascal_tls13_early_data.pas` 先补 runtime parity RED / fresh evidence。
3. 跑 focused test；若 fresh result 已天然 GREEN，则不伪造生产修复。
4. 更新 `docs/ROADMAP.md` / `docs/INTEGRATION_GUIDE.md`，把当前 shipped truth 和 opt-in 边界写清。
5. 跑 focused regression、capability wording regressions、completeness gate、compile gate、diff hygiene。
6. 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Task 1: RED - Lock Runtime Parity For Opt-In Replay Paths

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add a callback-helper runtime lifecycle parity contract**
- 新增 focused runtime contract，覆盖：
  - 两个 server contexts 通过 `InstallCallbackBackedReplayLedger(...)` 共用 callback store
  - first resumed early-data 继续 accept
  - 在其中一个 context 上执行 local lifecycle toggle（disable / re-enable 或 capacity `0 -> 8`）
  - second resumed early-data 仍必须 replay reject，而不是因为 local toggle 被隐式 wipe
  - resumed handshake 仍成功、session 仍 reused、discarded early bytes 不可读

**Step 2: Add a file-backed installer runtime lifecycle parity contract**
- 新增 focused runtime contract，覆盖：
  - 两个 server contexts 通过 `InstallFileBackedReplayLedger(...)` 指向同一个 replay-store file
  - first resumed early-data 继续 accept
  - local lifecycle toggle（disable / re-enable、capacity `0 -> 8`）之后
  - second resumed early-data 仍必须 replay reject
  - resumed handshake 仍成功、session 仍 reused、discarded early bytes 不可读

**Step 3: Keep scope tight**
- 只锁最小 runtime truth：
  - local gate disable 时 fresh acquire 不放行
  - gate 恢复后 replay truth 仍保留
  - 不新开 builder/config/runtime surface

**Step 4: Run focused RED / fresh evidence**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - 如果 runtime parity 还缺口，先出现 RED
  - 如果这条路径早已满足，则直接 GREEN；这应视为 fresh runtime closeout evidence

### Task 2: GREEN / DOC CLOSEOUT - Refresh User-Facing Truth

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `docs/INTEGRATION_GUIDE.md`

**Step 1: Refresh roadmap next-step wording**
- 在 `docs/ROADMAP.md` 当前 FreePascal early-data 区块补充：
  - default shipped truth 仍是 in-memory single-process ledger
  - file-backed replay-store opt-in 与 builder/config parity 已落地，但不升级 capability wording
  - managed seam boundary contracts 与 runtime parity evidence 已补齐
  - 下一条值得开的线仍是“更重的 provider/durability 形态验证”，不是重开当前 seam

**Step 2: Refresh integration guide boundary wording**
- 在 `docs/INTEGRATION_GUIDE.md` 现有 early-data / replay-store opt-in 小节补充：
  - default path 仍是 in-memory single-process anti-replay
  - callback/file-backed opt-in 的 local enabled/capacity toggle 不等于 wipe shared/persisted replay truth
  - file-backed store 是 opt-in seam，不是默认 durability 承诺
  - `experimental` capability wording 继续有效

**Step 3: Keep docs scope tight**
- 不改 capability string 本身
- 不引入新的 code snippets surface
- 不把 file-backed path 写成 default / stable / distributed-ready

### Task 3: Verify Adjacent Truth Stays Locked

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
- Reference: `tests/test_freepascal_backend_basic.pas`
- Reference: `tests/test_capability_cache.pas`
- Reference: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Re-run capability wording regressions**
- Run:
  - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
- Expected:
  - PASS
  - `KnownIssues` 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`

**Step 2: Run focused gate + compile gate**
- Run:
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_runtime_parity_doc_closeout_20260414`
  - `python3 scripts/compile_all_modules.py`

**Step 3: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-runtime-parity-and-doc-closeout.md docs/ROADMAP.md docs/INTEGRATION_GUIDE.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`

### Definition Of Done

- callback/file-backed opt-in paths 的 lifecycle / replay-retention runtime truth 有 fresh focused evidence
- 若 runtime contracts 天然为 GREEN，则本批不引入任何生产代码修改
- `docs/ROADMAP.md` / `docs/INTEGRATION_GUIDE.md` 与当前 shipped truth 对齐
- capability wording、public surface、context/connection wiring 都不变
- focused tests、capability wording tests、completeness gate、compile gate、diff hygiene 都有 fresh evidence
