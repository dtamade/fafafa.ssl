# FreePascal Certificate Store Contains Fingerprint Contract Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 FreePascal 证书存储 `Contains` 的语义缺口，使其不仅识别同接口引用，也能识别“同证书不同实例（同指纹）”。

**Architecture:** 先在 `test_freepascal_backend_basic` 增加 clone 证书 `Contains` 红测；再最小修改 `TFreePascalCertificateStore.Contains` 引入指纹匹配；最后执行回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.lib`, 程序级测试。

---

## Scan Summary (2026-02-11)

### High-signal gap
1. `src/fafafa.ssl.freepascal.lib.pas:903`
   - `Contains` 仅 `TInterfaceList.IndexOf` 判定，按对象引用比较。
2. `src/fafafa.ssl.freepascal.lib.pas:868`
   - `AddCertificate` 已支持指纹去重，但 `Contains` 仍是引用语义，接口行为不一致。
3. `tests/test_freepascal_backend_basic.pas`
   - 目前未覆盖 `Contains(LCert.Clone)` 的契约。

### Priority
- **P0:** Store `Contains` 指纹语义补齐（test-first）

---

### Task 1 (P0): Add failing test for clone contains contract

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertion**
- 在添加首个证书后增加断言：
  - `LStore.Contains(LCert.Clone)` 为 `True`
- 目标：Store 能识别同证书不同实例。

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前 Contains 为引用比较）。

---

### Task 2 (P0): Implement fingerprint-aware Contains

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- 在 `Contains` 中保留现有引用比较。
- 对引用不命中场景：
  - 取 `ACert.GetFingerprintSHA256`
  - 归一化后通过 `FindByFingerprint` 判断是否存在。

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

## Execution Record (2026-02-11 11:08 +0800)

### RED
- Command:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- Result: FAIL
- Key output:
  - `❌ Certificate store should treat cloned certificate with same fingerprint as contained`

### GREEN
- Code changes:
  - `tests/test_freepascal_backend_basic.pas`
    - 新增 `LStore.Contains(LCert.Clone)` 契约断言。
  - `src/fafafa.ssl.freepascal.lib.pas`
    - `Contains` 新增指纹匹配语义（保留引用匹配）。
- Command:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- Result: PASS
- Key output:
  - `✅ FreePascal backend basic checks passed`

### Regression
- Command:
  - `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept`
  - `./tmp/test_fp_accept`
- Result: PASS
- Key output:
  - `✅ FreePascal server accept skeleton checks passed`

- Command:
  - `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- Result: PASS
- Key output:
  - `Number of run tests: 10 / failures: 0 / errors: 0 / ignored: 2`

### Status
- Iteration 9 P0 task: complete
