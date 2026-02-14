# FreePascal Invalid Certificate Load Contract Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 FreePascal 证书加载接口对无效输入的语义缺口，确保 `LoadFromStream/LoadFromDER/LoadFromPEM` 对非证书数据返回 `False`。

**Architecture:** 先在 `test_freepascal_backend_basic` 增加无效流红测；再最小修改 `TFreePascalCertificate.LoadFromDER/LoadFromPEM` 在解析失败时返回 false 并清理内部状态；最后执行回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.lib`, `fafafa.ssl.x509`, 程序级测试。

---

## Scan Summary (2026-02-11)

### 全仓扫描结论（高信号）
1. `src/fafafa.ssl.freepascal.lib.pas:384`
   - `LoadFromDER` 对任意非空字节直接返回 `True`，未验证是否为合法证书。
2. `src/fafafa.ssl.freepascal.lib.pas:367`
   - `LoadFromPEM` 在 PEM 转 DER 后也未进行 X.509 解析成功判定。
3. `tests/test_freepascal_backend_basic.pas`
   - 目前仅覆盖“有效证书可加载”，未覆盖“无效输入应拒绝”的契约。

### Priority
- **P0:** 无效证书输入拒绝契约（test-first）。
- **P1:** 后续可扩展更多坏样本（损坏 PEM/截断 DER）夹具验证（本轮不展开）。

---

### Task 1 (P0): Add failing test for invalid cert data

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertion**
- 新增断言：
  - 使用 `TStringStream('not-a-certificate')`
  - `LInvalidCert.LoadFromStream(...)` 必须返回 `False`

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前会错误返回 `True`）。

---

### Task 2 (P0): Implement strict parse validation in certificate loaders

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- 在 `LoadFromDER`：
  - 先尝试 `TX509Certificate.LoadFromDER` 验证，失败则返回 `False`。
  - 失败时清空 `FDERData/FPEMData/FInfo`。
- 在 `LoadFromPEM`：
  - PEM->DER 后复用 `LoadFromDER` 路径；失败返回 `False`。
- 保持成功路径与现有行为兼容。

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

### Task 1 (P0): Add failing test for invalid cert data
- Modified: `tests/test_freepascal_backend_basic.pas`
- Added assertion:
  - `LInvalidCert.LoadFromStream(TStringStream('not-a-certificate'))` must return `False`.

**RED command**
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
**Output (key):**
- `❌ FreePascal certificate should reject invalid stream data`

### Task 2 (P0): Implement strict parse validation in certificate loaders
- Modified: `src/fafafa.ssl.freepascal.lib.pas`
- Implementation:
  - `LoadFromDER` now validates input via `TX509Certificate.LoadFromDER` before accepting data.
  - parse failure now clears certificate state and returns `False`.
  - `LoadFromPEM` now converts PEM->DER and reuses strict `LoadFromDER` path.

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
- Iteration 13 P0 task: **complete**
- Contract gap (invalid certificate input incorrectly accepted) closed.
