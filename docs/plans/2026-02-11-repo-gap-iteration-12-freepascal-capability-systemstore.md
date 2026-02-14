# FreePascal Capability System Store Contract Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 FreePascal backend `GetCapabilities.SupportsSystemCertStore` 与 `LoadSystemStore` 行为不一致缺口，使能力声明与可用功能对齐。

**Architecture:** 先在 `test_freepascal_backend_basic` 增加条件红测（系统证书目录存在时能力位必须为 true），再最小修改 `TFreePascalSSLLibrary.GetCapabilities` 按已实现目录检测赋值，最后执行回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.lib`, 程序级测试。

---

## Scan Summary (2026-02-11)

### 全仓扫描结论（高信号）
1. `src/fafafa.ssl.freepascal.lib.pas:996`
   - `LoadSystemStore` 已实现 Linux/Unix 常见系统证书目录加载。
2. `src/fafafa.ssl.freepascal.lib.pas:1311`
   - `GetCapabilities` 仍固定 `SupportsSystemCertStore := False`。
3. `tests/test_freepascal_backend_basic.pas`
   - 目前验证了 `LoadSystemStore` 行为，但未验证 capability bit 与该行为一致。

### Priority
- **P0:** 能力声明一致性：`SupportsSystemCertStore` 与 `LoadSystemStore` 保持一致（test-first）。
- **P1:** 后续可评估 `VerifyCertificate` 的链验证强度（本轮不展开）。
- **P2:** 后续可评估 `BuildCertificateChain` 的去环语义（本轮不展开）。

---

### Task 1 (P0): Add failing test for capability consistency

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertion**
- 在 capability 断言附近新增条件断言：
  - 若 `DirectoryExists('/etc/ssl/certs')`，则 `LCaps.SupportsSystemCertStore` 必须为 `True`。

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前 capability 固定为 `False`）。

---

### Task 2 (P0): Implement runtime capability flag

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- 在 `GetCapabilities` 中将 `SupportsSystemCertStore` 改为运行时检测：
  - 复用 `LoadSystemStore` 现有系统路径集合进行目录存在性判断。
  - 命中任一路径即为 `True`，否则 `False`。

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

## Execution Record (2026-02-11)

### Task 1 (P0): Add failing test for capability consistency
- Modified: `tests/test_freepascal_backend_basic.pas`
- Added assertion:
  - if `DirectoryExists('/etc/ssl/certs')`, then `LCaps.SupportsSystemCertStore` must be `True`.

**RED command**
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
**Output (key):**
- `❌ FreePascal capabilities should advertise system cert store support when Linux system store directory exists`

### Task 2 (P0): Implement runtime capability flag
- Modified: `src/fafafa.ssl.freepascal.lib.pas`
- Implementation:
  - Added shared `CSystemStorePaths` constant at unit scope.
  - Added helper `HasSystemCertStoreDirectories` for runtime path detection.
  - `LoadSystemStore` now reuses shared `CSystemStorePaths`.
  - `GetCapabilities` now sets `SupportsSystemCertStore := HasSystemCertStoreDirectories`.

**GREEN command**
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
**Output (key):**
- `✅ FreePascal backend basic checks passed`

### Task 3 (P1): Focused regression

**Regression command 1**
```bash
fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept
```
**Output (key):**
- `✅ FreePascal server accept skeleton checks passed`

**Regression command 2**
```bash
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
**Output (key):**
- `Number of run tests: 10`
- `Number of failures: 0`
- `Number of errors: 0`
- `Number of ignored tests: 2`

### Iteration Status
- Iteration 12 P0 task: **complete**
- Capability contract gap (`SupportsSystemCertStore`) closed.
