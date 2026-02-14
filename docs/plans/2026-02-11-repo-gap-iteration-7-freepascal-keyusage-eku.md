# FreePascal KeyUsage / EKU Contract Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 FreePascal 证书对象 `GetKeyUsage` 与 `GetExtendedKeyUsage` 的空实现，使其可返回可用用途列表并有回归测试。

**Architecture:** 新增带 `keyUsage` / `extendedKeyUsage` 扩展的测试证书夹具；在 `test_freepascal_backend_basic` 增加红测断言；然后最小实现 `TFreePascalCertificate.GetKeyUsage` 与 `GetExtendedKeyUsage`（复用 `TX509Certificate` 解析）；最后执行回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.lib`, `fafafa.ssl.x509`, 程序级测试。

---

## Scan Summary (2026-02-11)

### High-signal gap
1. `src/fafafa.ssl.freepascal.lib.pas:697`
   - `GetKeyUsage` 当前固定返回空数组。
2. `src/fafafa.ssl.freepascal.lib.pas:702`
   - `GetExtendedKeyUsage` 当前固定返回空数组。
3. `tests/test_freepascal_backend_basic.pas`
   - 尚未覆盖 KU/EKU 返回契约。

### Priority
- **P0:** KU/EKU 空实现收敛（test-first）

---

### Task 1 (P0): Add failing tests for KU/EKU contract

**Files:**
- Create: `tests/certificate/test_certs/keyusage_cert.pem`
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertions**
- 加载 `keyusage_cert.pem` 后断言：
  - `GetKeyUsage` 非空，且包含 `digitalSignature` 与 `keyEncipherment`
  - `GetExtendedKeyUsage` 非空，且包含 `serverAuth` 与 `clientAuth`

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前 KU/EKU 恒为空）。

---

### Task 2 (P0): Implement minimal KU/EKU extraction

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- 在 `GetKeyUsage`：
  - 解析证书后读取 `LParser.KeyUsage` 位集并映射字符串。
- 在 `GetExtendedKeyUsage`：
  - 解析证书后读取 `LParser.ExtKeyUsage` 位集并映射字符串。
- 解析失败时返回空数组，保持安全行为。

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

---

## Execution Record (2026-02-11 10:31 +0800)

### RED
- Command:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- Result: FAIL
- Key output:
  - `❌ KU fixture should expose non-empty key usage list`

### GREEN
- Code changes:
  - `tests/test_freepascal_backend_basic.pas`
    - 新增 KU/EKU fixture 断言（`digitalSignature`/`keyEncipherment`/`serverAuth`/`clientAuth`）。
  - `src/fafafa.ssl.freepascal.lib.pas`
    - 实现 `TFreePascalCertificate.GetKeyUsage`（解析 `TX509Certificate.KeyUsage` 位集映射字符串）。
    - 实现 `TFreePascalCertificate.GetExtendedKeyUsage`（解析 `TX509Certificate.ExtKeyUsage` 位集映射字符串）。
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
- Iteration 7 P0 task: complete
- Next candidate (P0): 继续收敛 FreePascal certificate API 其余契约（例如更精细的公钥内容/证书链语义一致性）或测试占位项清理。
