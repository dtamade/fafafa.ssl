# FreePascal Version String Skeleton Gap Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 清理 FreePascal backend 版本字符串中的 `skeleton` 占位标识，使对外元数据与当前实现状态一致，并建立防回归测试。

**Architecture:** 先在 `test_freepascal_backend_basic` 写失败断言锁定版本字符串契约（不得出现 `skeleton`）；再最小修改 `GetVersionString` 文案，不改功能路径；最后做小范围回归。

**Tech Stack:** FreePascal (ObjFPC), 程序级测试, `fafafa.ssl.freepascal.lib`。

---

## Scan Summary (2026-02-11)

### High-signal gap
1. `src/fafafa.ssl.freepascal.lib.pas:1052`
   - `GetVersionString` 返回 `FreePascal Native Backend (skeleton)`。
2. 当前 FreePascal backend 已具备 TLS1.3 核心握手与 CertificateVerify 路径，不应继续暴露 skeleton 占位元数据。

### Priority
- **P0:** 版本字符串契约修正（测试先行）

---

### Task 1 (P0): Add failing test for version string contract

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertions**
- 新增断言：
  - `LowerCase(LLib.GetVersionString)` 不包含 `skeleton`
  - 同时包含 `freepascal`

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前版本字符串含 skeleton）。

---

### Task 2 (P0): Minimal implementation for version string

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- 将 `GetVersionString` 文案从
  - `FreePascal Native Backend (skeleton)`
  - 改为 `FreePascal Native Backend (TLS 1.3 core path)`

**Step 2: Run test to verify GREEN**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: PASS。

---

### Task 3 (P1): Focused regression

**Files:**
- Verify only

**Step 1: Run server skeleton regression**
- Run:
  - `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept`
  - `./tmp/test_fp_accept`
- Expected: PASS。

**Step 2: Update planning files**
- 更新：`task_plan.md`、`findings.md`、`progress.md`。

---

## Execution Notes
- 严格遵循：不写脚本、不改 CI/DI。
- 严格 TDD：先 RED，再 GREEN，再回归。
- 每步命令输出回报。

## Suggested Immediate Start
- 立即执行 Task 1 RED。
