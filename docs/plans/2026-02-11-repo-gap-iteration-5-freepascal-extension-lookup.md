# FreePascal Certificate Extension Lookup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 补齐 FreePascal 证书对象 `GetExtension` 的空实现，使其能按 OID 返回扩展值，并建立回归测试。

**Architecture:** 先在 `test_freepascal_backend_basic` 增加失败断言，锁定 `2.5.29.14`（Subject Key Identifier）扩展读取契约；再对 `TFreePascalCertificate.GetExtension` 做最小实现（复用现有 `TX509Certificate` 解析）；最后执行 FreePascal 关键回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.lib`, `fafafa.ssl.x509`, 程序级测试。

---

## Scan Summary (2026-02-11)

### High-signal gap
1. `src/fafafa.ssl.freepascal.lib.pas:647-650`
   - `TFreePascalCertificate.GetExtension` 目前直接返回空字符串。
2. `tests/test_freepascal_backend_basic.pas`
   - 已覆盖证书加载和指纹，但未覆盖扩展提取能力。

### Priority
- **P0:** `GetExtension` 空实现收敛（test-first）

---

### Task 1 (P0): Add failing test for extension lookup contract

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertion**
- 在证书成功加载后新增断言：
  - `LCert.GetExtension('2.5.29.14') <> ''`
- 目标：要求 FreePascal backend 至少可返回 Subject Key Identifier 扩展值。

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前 `GetExtension` 恒为空）。

---

### Task 2 (P0): Implement minimal FreePascal extension lookup

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- 在 `GetExtension(const AOID: string)` 中：
  - 复用现有 DER/PEM -> DER 处理逻辑
  - 使用 `TX509Certificate` 解析扩展列表
  - 按 OID 匹配并返回扩展值（字节转十六进制字符串）
- 解析失败或无匹配时返回空串，保持安全行为。

**Step 2: Run test to verify GREEN**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: PASS。

---

### Task 3 (P1): Focused regression

**Files:**
- Verify only

**Step 1: Run FreePascal server accept regression**
- Run:
  - `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept`
  - `./tmp/test_fp_accept`
- Expected: PASS。

**Step 2: Run unit regression subset**
- Run:
  - `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple`
  - `./tmp/run_unit_tests_simple --format=plain --all`
- Expected: PASS。

**Step 3: Update planning files**
- 更新：`task_plan.md`、`findings.md`、`progress.md`。

---

## Execution Notes
- 严格遵循：不写脚本、不改 CI/DI。
- 严格 TDD：先 RED，再 GREEN，再回归。
- 每一步命令输出必须回报。

## Suggested Immediate Start
- 立即执行 Task 1 RED。
