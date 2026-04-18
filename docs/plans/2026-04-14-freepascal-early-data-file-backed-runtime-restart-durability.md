# FreePascal Early-Data File-Backed Runtime Restart Durability Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改 public API、builder/factory/config surface、`TFreePascalContext` / `TFreePascalConnection` wiring 与 capability wording 的前提下，为 FreePascal early-data file-backed anti-replay opt-in 路径补一条真实的“跨进程 / 重启后 replay truth 仍保留”的 runtime durability 合同。

**Architecture:** 继续复用 backend-private file-backed installer seam、现有 scripted resumed early-data runtime harness、`TFreePascalSession.Serialize/Deserialize` 与 child-process 自执行模式。第一优先级是在 `tests/test_freepascal_tls13_early_data.pas` 上补 1 个 installer-based restart runtime contract：父进程先接受一次 resumed early-data 并把 session 序列化落盘；子进程复用同一个 replay-store 文件与 session 文件，证明 replay reject 仍成立，同时 fresh resumed session 仍可 accept。除非 fresh RED 暴露真实生产缺口，否则本批不改 production units。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data scripted runtime tests, backend-private installer seam, file-backed replay-store provider, child-process self-exec, file-based working memory.

---

## Summary

- 当前 file-backed truth 已经覆盖：
  - direct provider persistence / rebuild
  - cross-context replay reject
  - installer/runtime parity
  - cross-process lock-contention fail-closed
- 当前最高 ROI 剩余缺口是：
  - 真实 resumed early-data runtime path 下，**新进程 / 重启边界**是否继续保留 replay truth
- 本批明确不做：
  - capability wording 升级
  - builder/config/runtime 新 surface
  - distributed / default durable anti-replay
  - `context.pas` / `connection.pas` 结构调整

## Delivery Order

1. 写本轮 plan 与 working-memory 入口。
2. 在 `tests/test_freepascal_tls13_early_data.pas` 先补 1 个 child-mode helper + 1 个 installer-based restart runtime RED。
3. 跑 focused test；若 fresh result 直接 GREEN，则不伪造生产修复。
4. 仅当 fresh RED 证明 provider/runtime 仍有缺口时，最小查看并修改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
5. 跑 focused regression、capability wording regressions、completeness gate、compile gate、diff hygiene。
6. 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Task 1: RED - Lock Runtime Restart Durability

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`
- Reference: `src/fafafa.ssl.freepascal.session.pas`

**Step 1: Add a child-mode restart helper**
- 在 focused test 文件里增加最小 child helper，负责：
  - 读取父进程写出的 serialized session 文件
  - 创建全新的 FreePascal server context
  - 安装同一个 file-backed replay ledger
  - 把 deserialize 后的 session 存回 resumption cache
  - 对 replayed resumed early-data 断言 reject
  - 在同一 child 进程里再 capture 一个 fresh session，并断言 first resumed early-data 仍 accept

**Step 2: Add an installer-based restart runtime contract**
- 新增 focused runtime contract，覆盖：
  - 父进程 file-backed installer path 先 accept 一次 resumed early-data
  - replay-store file 与 serialized session file 都成功 materialize
  - 子进程复用同一个 replay-store file 后，对同一 session 的 resumed early-data 必须 reject
  - 同一子进程中的 fresh resumed session 仍 accept

**Step 3: Keep scope tight**
- 只锁真实 restart durability truth：
  - replay truth 穿过进程边界仍可见
  - persisted state 不会“毒化”fresh session
  - 不新开 builder/config/runtime surface

**Step 4: Run focused RED / fresh evidence**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - 如果 restart durability 还有缺口，先出现 RED
  - 如果 file-backed runtime path 已天然满足，则直接 GREEN；这应视为 fresh runtime durability closeout evidence

### Task 2: GREEN - Only If RED Proves A Real Gap

**Files:**
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Keep production scope minimal**
- 若 fresh RED 暴露 restart durability 真缺口：
  - 优先只改 file-backed provider / store 实现
  - 不先碰 `context.pas` / `connection.pas`
  - 不改 public API、builder/factory/config surface

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
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/freepascal_backend_basic/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
- Expected:
  - PASS
  - `KnownIssues` 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`

**Step 2: Run focused gate + compile gate**
- Run:
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_file_backed_runtime_restart_durability_20260414`
  - `python3 scripts/compile_all_modules.py`

**Step 3: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-14-freepascal-early-data-file-backed-runtime-restart-durability.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`

### Definition Of Done

- file-backed installer/runtime path 的 replay truth 有 fresh “跨进程 / 重启后仍 reject replay” evidence
- 同一 child 进程里的 fresh resumed session 继续 accept，证明 persisted state 没有污染 fresh truth
- 若 runtime contract 天然为 GREEN，则本批不引入任何 production code 修改
- capability wording、public surface、context/connection wiring 都不变
- focused tests、capability wording tests、completeness gate、compile gate、diff hygiene 都有 fresh evidence
