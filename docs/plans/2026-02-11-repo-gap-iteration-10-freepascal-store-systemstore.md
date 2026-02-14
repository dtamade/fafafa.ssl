# FreePascal Certificate Store LoadSystemStore Gap Closure Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 FreePascal 证书存储 `LoadSystemStore` 恒返回 `False` 的缺口，在存在系统证书目录时能实际加载并返回成功。

**Architecture:** 在 `test_freepascal_backend_basic` 增加基于目录存在性的红测；再最小实现 `TFreePascalCertificateStore.LoadSystemStore` 复用 `LoadFromPath` 扫描常见系统证书目录；最后执行回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.lib`, 程序级测试。

---

## Scan Summary (2026-02-11)

### High-signal gap
1. `src/fafafa.ssl.freepascal.lib.pas:977`
   - `LoadSystemStore` 当前仅 `Result := False`，没有任何加载逻辑。
2. `tests/test_freepascal_backend_basic.pas`
   - 目前未覆盖系统证书加载契约。

### Priority
- **P0:** `LoadSystemStore` 可用性补齐（test-first）

---

### Task 1 (P0): Add failing test for system store loading

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertion**
- 在现有 Store 测试后新增条件断言：
  - 若 `DirectoryExists('/etc/ssl/certs')`，则 `LStore.LoadSystemStore` 必须返回 `True`。
- 目标：在典型 Linux 环境暴露当前空实现。

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前实现恒 False）。

---

### Task 2 (P0): Implement minimal system store loader

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- 在 `LoadSystemStore` 中遍历常见系统证书目录（如 `/etc/ssl/certs`、`/etc/pki/tls/certs`）。
- 目录存在时调用 `LoadFromPath`，任一目录加载成功即返回 `True`。
- 若目录都不存在或加载失败，返回 `False`。

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

---

## Execution Record (2026-02-11 11:13 +0800)

### RED
- Command:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- Result: FAIL
- Key output:
  - `❌ LoadSystemStore should load certificates when Linux system store directory exists`

### GREEN
- Code changes:
  - `tests/test_freepascal_backend_basic.pas`
    - 在 `/etc/ssl/certs` 存在时增加 `LoadSystemStore` 成功断言。
  - `src/fafafa.ssl.freepascal.lib.pas`
    - `LoadSystemStore` 实现常见系统证书目录遍历加载逻辑。
- Command:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- Result: PASS
- Key output:
  - `✅ FreePascal backend basic checks passed`

### Regression
- Command:
  - `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept`
- Result: PASS
- Key output:
  - `✅ FreePascal server accept skeleton checks passed`

- Command:
  - `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- Result: PASS
- Key output:
  - `Number of run tests: 10 / failures: 0 / errors: 0 / ignored: 2`

### Status
- Iteration 10 P0 task: complete
