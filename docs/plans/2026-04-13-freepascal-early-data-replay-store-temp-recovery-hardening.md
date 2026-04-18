# FreePascal Early-Data Replay-Store Temp-Recovery Hardening Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改变默认 shipped behavior、public API 与 capability wording 的前提下，为 FreePascal TLS 1.3 early-data file-backed anti-replay provider 增加最小 orphan temp-file recovery hardening。

**Architecture:** 继续复用现有 `TFreePascalFileEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger` 与 backend-private installer seam，不新增新的 provider abstraction。最小修法是在 file-backed provider 内部把 `FFileName + '.tmp'` 视为 interrupted temp-file replace 的恢复候选：主 store 缺失时优先读取 orphan temp file，保持 live replay truth 与 fail-closed corruption semantics；成功写回时仍然落到 canonical main store file。

**Tech Stack:** FreePascal (ObjFPC), `TFileStream`, TLS 1.3 early-data runtime contracts, focused provider rebuild/corruption tests, focused completeness gate, file-based working memory.

---

## Summary

- 当前 live truth 已经锁住：
  - main replay-store file 的 persistence / corruption / expired-prune / cross-context replay
  - backend-private installer seam 与 builder/config/factory replay-store parity
- 当前高 ROI 剩余缺口：
  - file-backed provider 采用 `temp file + RenameFile`
  - 但如果进程在“temp 写完、main replace 未完成”之间中断，orphan `.tmp` 里的 replay truth 目前不会被恢复
- 本批只做最小 durability hardening：
  - main file 缺失且 `.tmp` 存在时，把 `.tmp` 视为 recovery candidate
  - live entry 仍要 reject replay
  - corrupt / invalid orphan temp store 仍要 fail closed
  - 不改 public surfaces / builder / factory / capability wording

## Delivery Order

1. 写本轮 plan 与 working-memory 入口。
2. 先在 `tests/test_freepascal_tls13_early_data.pas` 加 RED，锁住 orphan temp-file replay truth 与 fail-closed corruption semantics。
3. 最小 GREEN：只改 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
4. 跑 focused regression、focused gate、compile gate、diff hygiene。
5. 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Task 1: RED - Lock orphan temp-file recovery contracts

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Add focused orphan-temp replay contract**
- 新增 direct provider contract，覆盖：
  - main replay-store file 不存在
  - 只有 `AFileName + '.tmp'` 存在，且里面是 live replay entry
  - provider rebuild 后，对同一个 session 的 acquire 必须失败
  - 同时 fresh session 仍可成功 acquire，并 materialize canonical main store file

**Step 2: Add focused orphan-temp corruption contract**
- 新增 direct provider contract，覆盖：
  - main replay-store file 不存在
  - 只有 orphan `.tmp` store 存在，且 version invalid 或内容 truncated
  - provider 必须 fail closed，而不是忽略 `.tmp` 后静默 accept

**Step 3: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - FAIL，优先失败在“当前 provider 忽略 orphan temp file，导致 replay 被错误 accept”

### Task 2: GREEN - Recover orphan temp replay truth with minimal provider changes

**Files:**
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Add a bounded recovery-file resolver**
- 在 provider 内部新增最小 private helper，负责：
  - main file 存在 => 继续读 main file
  - main file 缺失且 `.tmp` 存在 => 读 `.tmp`
  - 两者都不存在 => empty store
- 不改变外部构造参数与 public contract

**Step 2: Keep fail-closed semantics aligned**
- orphan `.tmp` 一旦损坏 / 版本错误 / 截断：
  - `LoadEntries(...)` 仍返回 `False`
  - `TryAcquireReplayKey(...)` 仍 fail closed
- 不允许因为主文件缺失就绕过 temp-file corruption

**Step 3: Preserve canonical write path**
- 成功 acquire 后仍通过现有 `SaveEntries(...)` 把 fresh state 写回 canonical main file
- 不改 temp-file replace 主路径
- 不新增 backup/dual-write 机制

**Step 4: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Verify adjacent truth stays locked

**Files:**
- Reference: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Run focused verification**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_replay_store_temp_recovery_20260413`
  - `python3 scripts/compile_all_modules.py`

**Step 2: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-13-freepascal-early-data-replay-store-temp-recovery-hardening.md src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`

### Definition Of Done

- orphan `.tmp` replay store 在主文件缺失时不再被忽略
- live replay truth 可从 orphan temp store 恢复并继续 reject replay
- corrupt orphan temp store 保持 fail closed
- canonical main replay-store write path 继续不变
- focused test、focused gate、compile gate、diff hygiene 都有 fresh evidence
- public API、builder/factory surface 与 capability wording 都不变
