# WolfSSL SAN Extraction Coverage Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为 WolfSSL 证书 SAN 提取补齐明确覆盖，避免 SAN 能力回退或回归无感知。

**Architecture:** 在 `test_wolfssl_metadata_accuracy` 新增 SAN fixture 断言制造 RED；若现有实现不足，最小修改 `GetSubjectAltNames`，优先 wolfSSL API，必要时 fallback 到 DER 解析；最后做回归。

**Tech Stack:** FreePascal (ObjFPC), `src/fafafa.ssl.wolfssl.certificate.pas`, `tests/connection/test_wolfssl_metadata_accuracy.pas`.

---

### Task 1 (P1): Add SAN coverage failing test

**Files:**
- Modify: `tests/connection/test_wolfssl_metadata_accuracy.pas`

**Step 1: Write failing assertions**
- runtime + SAN API 可用时：加载 `tests/certs/san-test.pem`，断言可提取至少两个 DNS SAN 并包含 `san-test.local` 与 `example.test`。

**Step 2: Run RED command**
- `fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy`

---

### Task 2 (P1): Minimal fix for SAN extraction robustness

**Files:**
- Modify: `src/fafafa.ssl.wolfssl.certificate.pas` (if RED fails)

**Step 1: Minimal implementation**
- 保持 wolfSSL API 路径优先。
- 仅当 API 未返回 SAN 时，fallback 到已存在 DER 解析路径。

**Step 2: Run GREEN command**
- 同 RED 命令，预期 PASS 或显式 SKIP。

---

### Task 3 (P1): Focused regression

- `fpc -Fu./src tests/test_wolfssl_framework.pas -otmp/test_wolfssl_framework && ./tmp/test_wolfssl_framework`
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`


---

## Execution Record (2026-02-11 11:03 +0800)

### Task 1 (P1): Add SAN coverage test
- Modified: `tests/connection/test_wolfssl_metadata_accuracy.pas`
- Added:
  - SAN fixture check on `tests/certs/san-test.pem`
  - assertions for `san-test.local` and `example.test`

### RED Command (observed behavior)
```bash
fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy
```
- Output (key):
  - `✅ wolfssl metadata accuracy tests passed`

### Interpretation
- 本轮新增红测未出现失败，说明当前 `GetSubjectAltNames` 在本环境与 fixture 上已满足断言。
- 按 TDD 规则：该任务归类为“coverage hardening / regression guard”，无需实现代码变更。

### Regression
```bash
fpc -Fu./src tests/test_wolfssl_framework.pas -otmp/test_wolfssl_framework && ./tmp/test_wolfssl_framework
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
- Output (key):
  - `WolfSSL Framework Test Summary ... Failed: 0`
  - `✅ FreePascal backend basic checks passed`

### Status
- `P1-20`: complete (coverage)
- Next candidate: `P1-21 WolfSSL date metadata decoding robustness`
