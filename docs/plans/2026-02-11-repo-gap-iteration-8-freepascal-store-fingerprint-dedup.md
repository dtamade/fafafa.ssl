# FreePascal Certificate Store Fingerprint Dedup Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 FreePascal 证书存储 `AddCertificate` 的重复判定，仅拒绝“同引用”而放行“同证书不同实例”的缺口，保证按指纹去重。

**Architecture:** 在 `test_freepascal_backend_basic` 增加“克隆证书重复添加应失败”的红测；然后最小修改 `TFreePascalCertificateStore.AddCertificate` 引入指纹判重；最后执行回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.lib`, 程序级测试。

---

## Scan Summary (2026-02-11)

### High-signal gap
1. `src/fafafa.ssl.freepascal.lib.pas:868`
   - `AddCertificate` 仅通过 `Contains`（接口引用）去重。
2. `src/fafafa.ssl.freepascal.lib.pas:897`
   - `Contains` 使用 `TInterfaceList.IndexOf`，无法识别“同证书不同对象”。
3. `tests/test_freepascal_backend_basic.pas`
   - 仅覆盖“同引用重复添加失败”，未覆盖“同指纹不同实例重复添加失败”。

### Priority
- **P0:** Store fingerprint 去重契约（test-first）

---

### Task 1 (P0): Add failing test for fingerprint-based dedup

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertion**
- 在首个证书加入后，新增断言：
  - `not LStore.AddCertificate(LCert.Clone)`
- 目标：要求 Store 按证书指纹而非对象地址去重。

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前实现会错误接受 clone）。

---

### Task 2 (P0): Implement minimal fingerprint dedup in AddCertificate

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- 在 `AddCertificate` 中新增：
  - 读取 `ACert.GetFingerprintSHA256`
  - 归一化后使用 `FindByFingerprint` 判重
  - 命中即返回 `False`
- 保留现有引用去重逻辑。

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

## Execution Record (2026-02-11 10:44 +0800)

### RED
- Command:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- Result: FAIL
- Key output:
  - `❌ Certificate store should reject duplicate certificate fingerprints across cloned instances`

### GREEN
- Code changes:
  - `tests/test_freepascal_backend_basic.pas`
    - 新增 `not LStore.AddCertificate(LCert.Clone)` 断言。
  - `src/fafafa.ssl.freepascal.lib.pas`
    - `AddCertificate` 新增 SHA256 指纹判重路径（保留引用判重）。
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
- Iteration 8 P0 task: complete
