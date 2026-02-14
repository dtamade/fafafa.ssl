# WolfSSL Certificate IsCA BasicConstraints Accuracy Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 修复 `TWolfSSLCertificate.IsCA` 恒为 `False` 的占位行为，使其与证书 BasicConstraints 语义一致。

**Architecture:** 在 wolfssl metadata 测试中新增 CA/leaf 判定红测；最小实现 `IsCA` 通过现有 DER 数据回退到 `TX509Certificate` 解析 BasicConstraints；最后执行 wolfssl + 基础回归。

**Tech Stack:** FreePascal (ObjFPC), `src/fafafa.ssl.wolfssl.certificate.pas`, `tests/connection/test_wolfssl_metadata_accuracy.pas`, `fafafa.ssl.x509`.

---

### Task 1 (P1): Add failing IsCA metadata test

**Files:**
- Modify: `tests/connection/test_wolfssl_metadata_accuracy.pas`

**Step 1: Write failing assertion**
- runtime 可用时：
  - 加载 `tests/certificate/test_certs/ca_cert.pem`，断言 `IsCA = True`
  - 加载 `tests/certs/server-cert.pem`，断言 `IsCA = False`

**Step 2: Run RED command**
- `fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy`
- Expected: FAIL（当前 `IsCA` 恒为 `False`）。

---

### Task 2 (P1): Implement IsCA basic-constraints check

**Files:**
- Modify: `src/fafafa.ssl.wolfssl.certificate.pas`

**Step 1: Minimal implementation**
- `IsCA` 不再返回常量。
- 使用 `SaveToDER` + `TX509Certificate.LoadFromDER` 解析并返回 `Parser.IsCA`。
- 解析失败保持安全返回 `False`。

**Step 2: Run GREEN command**
- `fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy`
- Expected: PASS（或 runtime 不可用时显式 SKIP）。

---

### Task 3 (P1): Focused regression

- `fpc -Fu./src tests/test_wolfssl_framework.pas -otmp/test_wolfssl_framework && ./tmp/test_wolfssl_framework`
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`

**Expected:** PASS。

---

## Execution Notes
- 严格 TDD：RED -> GREEN -> Regression。
- 不写脚本，不改 CI/DI。
- 每步命令输出必须回报。

---

## Execution Record (2026-02-11 10:58 +0800)

### Task 1 (P1): Add failing IsCA metadata test
- Modified: `tests/connection/test_wolfssl_metadata_accuracy.pas`
- RED command:
```bash
fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy
```
- Output (key):
  - `❌ CA certificate should report IsCA=True`

### Task 2 (P1): Implement IsCA basic-constraints check
- Modified: `src/fafafa.ssl.wolfssl.certificate.pas`
- Implementation:
  - `IsCA` 改为 `SaveToDER + TX509Certificate.LoadFromDER + Parser.IsCA`。
  - 解析异常时安全返回 `False`。
- GREEN command:
```bash
fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy
```
- Output (key):
  - `✅ wolfssl metadata accuracy tests passed`

### Task 3 (P1): Focused regression
```bash
fpc -Fu./src tests/test_wolfssl_framework.pas -otmp/test_wolfssl_framework && ./tmp/test_wolfssl_framework
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
- Output (key):
  - `WolfSSL Framework Test Summary ... Failed: 0`
  - `✅ FreePascal backend basic checks passed`
  - `Number of run tests: 10 / failures: 0 / errors: 0`

### Status
- `P1-19`: complete
- Next candidate: `P1-20 WolfSSL SAN extraction coverage`
