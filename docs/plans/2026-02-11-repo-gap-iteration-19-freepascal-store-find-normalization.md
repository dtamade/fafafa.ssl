# FreePascal Store Find Normalization Plan (Subject/Serial)

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 FreePascal cert store 的 `FindBySubject/FindBySerialNumber` 查询语义，使其对常见输入格式差异（空白/分隔符/大小写）稳定匹配。

**Architecture:** 在 `test_freepascal_backend_basic` 增加红测（subject 空白变体 + serial 冒号/空白变体）；再最小修改 store 查找函数引入规范化；最后回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.lib`, 程序级测试。

---

## Scan Summary (2026-02-11)

### High-signal gap
1. `FindBySubject` 当前为 `SameText(LCert.GetSubject, ASubject)`，不处理格式噪声。
2. `FindBySerialNumber` 当前为 `SameText(LCert.GetSerialNumber, ASerialNumber)`，不处理 `:`/空白 等表示差异。
3. 调用侧在不同来源（日志、CLI、人工输入）下常会出现分隔符与空白差异。

### Priority
- **P0:** find 语义规范化（test-first）。

---

### Task 1 (P0): Add failing tests for normalized subject/serial lookup

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertions**
- 在 store 已含 `LCert` 时新增：
  - subject：构造“大小写+分隔符周边空白”变体，断言 `FindBySubject` 可匹配。
  - serial：构造“冒号分隔+首尾空白”变体，断言 `FindBySerialNumber` 可匹配。

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前为严格文本比较）。

---

### Task 2 (P0): Implement normalization in FindBySubject/FindBySerialNumber

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- `FindBySubject`：规范化处理（`Trim` + 分隔符周边空白收敛 + `UpperCase`）。
- `FindBySerialNumber`：仅保留十六进制字符并 `UpperCase` 后比较。

**Step 2: Run test to verify GREEN**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: PASS。

---

### Task 3 (P1): Focused regression

**Step 1:**
- `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept`
- `./tmp/test_fp_accept`

**Step 2:**
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple`
- `./tmp/run_unit_tests_simple --format=plain --all`

---

## Execution Notes
- 严格 TDD：先 RED，再 GREEN，再回归。
- 每一步命令输出必须回报。
- 不新增脚本，不改 CI/DI。

---

## Execution Record (2026-02-11 10:42 +0800)

### Task 1 (P0): Add failing tests for normalized subject/serial lookup
- Modified: `tests/test_freepascal_backend_basic.pas`
- Added assertions:
  - `FindBySubject` accepts subject query with case + delimiter whitespace variance
  - `FindBySerialNumber` accepts serial query with `:` separators + surrounding whitespace

**RED command**
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
**Output (key):**
- `❌ Certificate store should find certificate by normalized subject query`

### Task 2 (P0): Implement normalization in FindBySubject/FindBySerialNumber
- Modified: `src/fafafa.ssl.freepascal.lib.pas`
- Implementation:
  - `FindBySubject` 增加 `NormalizeSubject`（trim + 分隔符空白收敛 + uppercase）
  - `FindBySerialNumber` 增加 `NormalizeSerial`（仅十六进制字符 + uppercase）

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
- Iteration 19 P0 task: **complete**
- Contract gap (`FindBySubject/FindBySerialNumber` normalization) closed.
