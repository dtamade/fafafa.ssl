# Repo Gap Iteration 30 (P2-44 + P2-50) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 关闭 Linux 环境下可执行的最后两个 P2 收口项：`P2-44`（helper skip 分组简化）与 `P2-50`（Windows 批次闭环清单回写）。

**Architecture:** 先在 `test_helper_utilities` 通过 RED→GREEN 将 OpenSSL unavailable 路径统一为 group-level skip helper，并增加 skipped 计数与摘要；再将闭环执行协议回写到 `AUTONOMOUS_ITERATION_PROTOCOL` 与状态矩阵，明确 Windows 批次入口命令与验收标准。

**Tech Stack:** FreePascal (ObjFPC), test programs in `tests/`, markdown planning artifacts.

---

### Task 1 (P2-44): OpenSSL helper utility skip grouping simplification

**Files:**
- Modify: `tests/test_helper_utilities.pas`

**Step 1: RED（引入未实现 helper）**
- 修改一处 OpenSSL unavailable 分支为 `SkipOpenSSLGroup(...)`，不先实现 helper。
- Run:
  - `fpc -Fu./src tests/test_helper_utilities.pas -otmp/test_helper_utils`
- Expected: FAIL（`Identifier not found "SkipOpenSSLGroup"`）。

**Step 2: GREEN（最小实现）**
- 实现 `SkipOpenSSLGroup`，并将各组 `OpenSSL not available` 分支统一接入 helper。
- 增加 `SkippedTests` 统计与 summary 输出，保持失败出口语义不变。
- Run:
  - `fpc -Fu./src tests/test_helper_utilities.pas -otmp/test_helper_utils && ./tmp/test_helper_utils`
- Expected: PASS（OpenSSL 可用时 `Skipped: 0`，不可用时 group-level skip 计数稳定）。

**Step 3: Regression**
- Run:
  - `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- Expected: PASS。

---

### Task 2 (P2-50): Closure checklist and recurring execution protocol writeback

**Files:**
- Modify: `docs/plans/AUTONOMOUS_ITERATION_PROTOCOL.md`
- Modify: `docs/test_reports/REPO_GAP_TASK_STATUS_MATRIX_2026-02-11.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Write closure checklist**
- 在协议文档写入 `Repo Gap 50-task closure checklist`：
  - 已完成范围（P0/P1/P2 Linux 可执行项）
  - 阻塞范围（P1-33~P1-36）
  - Windows 批次执行命令（RED/GREEN/Regression）
  - 通过标准与回写位置

**Step 2: Update status matrix**
- 标记 `P2-44` / `P2-50` 为 `✅ Complete`。
- 更新 summary 与 next execution order 到“仅剩 WinSSL blocked batch”。

**Step 3: Planning-with-files writeback**
- 回写 `task_plan.md` / `findings.md` / `progress.md` 的本轮执行记录与 blocker 入口。

---

## Execution Record (2026-02-11)

### Task 1 RED
- Command:
  - `fpc -Fu./src tests/test_helper_utilities.pas -otmp/test_helper_utils`
- Result: FAIL
- Key output:
  - `Error: Identifier not found "SkipOpenSSLGroup"`

### Task 1 GREEN
- Command:
  - `fpc -Fu./src tests/test_helper_utilities.pas -otmp/test_helper_utils && ./tmp/test_helper_utils`
- Result: PASS
- Key output:
  - `Test Results: 24/24 passed (100.0%)`
  - `Skipped: 0`

### Task 1 Regression
- Command:
  - `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- Result: PASS
- Key output:
  - `Summary ... Passed: 5 / Failed: 0 / Skipped: 1`
- Command:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- Result: PASS
- Key output:
  - `✅ FreePascal backend basic checks passed`

### Task 2
- Result: complete
- Key output:
  - P2-44 and P2-50 are marked complete in status matrix
  - Remaining scope: P1-33~P1-36 blocked on Windows runtime
