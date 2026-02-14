# FreePascal Invalid DER/PEM Hardening Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 FreePascal 证书加载对“伪 PEM 块类型”和“截断 DER”的容错边界，确保无效输入被稳定拒绝。

**Architecture:** 在 `test_freepascal_backend_basic` 先加入红测（错误 PEM block type + truncated DER）；再最小修改 `LoadFromPEM` 仅接受 `CERTIFICATE` 块并走现有 DER 校验路径；最后回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.lib`, 程序级测试。

---

## Scan Summary (2026-02-11)

### High-signal gap
1. `src/fafafa.ssl.freepascal.lib.pas` `LoadFromPEM` 当前直接 `PEMToDER(APEM)`。
2. `TSSLUtils.IsPEMFormat/PEMToDER` 只校验 BEGIN/END 通用标记，不约束 PEM block type。
3. 结果：错误 block type（例如 `BEGIN PUBLIC KEY`）只要 base64 可解且字节可被解析，存在误接收风险。

### Priority
- **P0:** 输入有效性语义收敛（test-first）。

---

### Task 1 (P0): Add failing tests for invalid PEM block type and truncated DER

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertions**
- 在无效输入场景新增：
  - 基于有效证书 PEM，替换为 `BEGIN/END PUBLIC KEY` 后断言 `LoadFromPEM` 返回 `False`。
  - 对有效 DER 截断（保留前 N 字节）后断言 `LoadFromDER` 返回 `False`。

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前错误 PEM block type 仍可能被接受）。

---

### Task 2 (P0): Restrict PEM loader to CERTIFICATE block

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- `LoadFromPEM` 先提取 `CERTIFICATE` 块（`ExtractPEMBlock(..., 'CERTIFICATE')`）。
- 若无证书块/空块则直接失败并保持状态清理。
- 对提取后的证书块再执行 `PEMToDER -> LoadFromDER`。

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

## Execution Record (2026-02-11 10:18 +0800)

### Task 1 (P0): Add failing tests for invalid PEM block type and truncated DER
- Modified: `tests/test_freepascal_backend_basic.pas`
- Added assertions:
  - `LoadFromDER` rejects truncated DER from valid cert bytes
  - `LoadFromPEM` rejects wrong PEM block type (`PUBLIC KEY`)

**RED command**
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
**Output (key):**
- `❌ FreePascal certificate should reject PEM payload without CERTIFICATE block type`

### Task 2 (P0): Restrict PEM loader to CERTIFICATE block
- Modified: `src/fafafa.ssl.freepascal.lib.pas`
- Implementation:
  - `LoadFromPEM` now extracts `CERTIFICATE` block first (`ExtractPEMBlock`) and rejects empty/missing block
  - decode+DER validation continues via `LoadFromDER`

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
- Iteration 17 P0 task: **complete**
- Contract gap (`invalid PEM block type acceptance`) closed; invalid-input hardening coverage improved.
