# FreePascal Certificate Store Remove Fingerprint Contract Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 FreePascal 证书存储 `RemoveCertificate` 的语义缺口，使其支持按指纹移除“同证书不同实例（Clone）”。

**Architecture:** 在 `test_freepascal_backend_basic` 增加 clone 移除红测；然后最小修改 `TFreePascalCertificateStore.RemoveCertificate` 在引用未命中时走指纹匹配删除；最后执行回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.lib`, 程序级测试。

---

## Scan Summary (2026-02-11)

### High-signal gap
1. `src/fafafa.ssl.freepascal.lib.pas:887`
   - `RemoveCertificate` 仅按 `TInterfaceList.IndexOf`（引用比较）移除。
2. `src/fafafa.ssl.freepascal.lib.pas:903`
   - `Contains` 已支持引用 + 指纹双语义；`RemoveCertificate` 与其不一致。
3. `tests/test_freepascal_backend_basic.pas`
   - 当前未覆盖 `RemoveCertificate(LCert.Clone)` 的行为契约。

### Priority
- **P0:** Store `RemoveCertificate` 指纹语义补齐（test-first）

---

### Task 1 (P0): Add failing test for clone remove contract

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertion**
- 在添加证书后新增断言：
  - `LStore.RemoveCertificate(LCert.Clone)` 为 `True`
  - 移除后 `LStore.GetCount = 0`
  - 可重新添加原证书

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前 `RemoveCertificate` 仅引用匹配）。

---

### Task 2 (P0): Implement fingerprint-aware RemoveCertificate

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- 保留现有引用匹配删除路径。
- 引用未命中时：
  - 归一化 `ACert.GetFingerprintSHA256`
  - 使用 `FindByFingerprint` 定位已有证书
  - 找到后删除对应索引并返回 `True`

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

### Task 1 (P0): Add failing test for clone remove contract
- Modified: `tests/test_freepascal_backend_basic.pas`
- Added assertions:
  - `LStore.RemoveCertificate(LCert.Clone)` must be `True`
  - `LStore.GetCount = 0` after clone removal
  - original cert can be added again after removal

**RED command**
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
**Output (key):**
- `❌ Certificate store should remove cloned certificate by matching fingerprint`

### Task 2 (P0): Implement fingerprint-aware `RemoveCertificate`
- Modified: `src/fafafa.ssl.freepascal.lib.pas`
- Implementation:
  - keep existing reference-based deletion path
  - fallback to fingerprint match when reference index not found
  - delete matched store entry and return `True`

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
- Iteration 11 P0 task: **complete**
- Contract gap (`RemoveCertificate` clone fingerprint semantics) closed.
