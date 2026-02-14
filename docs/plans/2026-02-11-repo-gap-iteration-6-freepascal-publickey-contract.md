# FreePascal Certificate PublicKey Contract Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 FreePascal 证书对象 `GetPublicKey` 的空返回行为，确保已加载证书可返回非空公钥信息字符串并具备回归测试。

**Architecture:** 先在 `test_freepascal_backend_basic` 增加失败断言锁定契约，再对 `TFreePascalCertificate.GetPublicKey` 做最小实现（与 OpenSSL backend 一致返回算法标识），最后执行关键回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.lib`, 程序级测试。

---

## Scan Summary (2026-02-11)

### High-signal gap
1. `src/fafafa.ssl.freepascal.lib.pas:510-513`
   - `TFreePascalCertificate.GetPublicKey` 当前固定返回空字符串。
2. `tests/test_freepascal_backend_basic.pas`
   - 已覆盖证书加载、指纹、扩展，但未覆盖 `GetPublicKey` 契约。

### Priority
- **P0:** `GetPublicKey` 空返回收敛（test-first）

---

### Task 1 (P0): Add failing test for GetPublicKey contract

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertion**
- 在证书加载成功后新增断言：
  - `LCert.GetPublicKey <> ''`
- 目标：已加载证书必须暴露非空公钥信息。

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前 `GetPublicKey` 恒为空）。

---

### Task 2 (P0): Implement minimal GetPublicKey behavior

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- 将 `GetPublicKey` 调整为返回 `GetPublicKeyAlgorithm`。
- 与 OpenSSL backend 的简化行为保持一致，不额外引入复杂导出逻辑。

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
