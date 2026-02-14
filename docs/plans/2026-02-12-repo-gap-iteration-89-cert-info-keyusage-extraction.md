# CertificateUtils GetInfo KeyUsage Extraction Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 补齐 `TCertificateUtils.GetInfo` 中 `KeyUsage` 元数据提取，避免证书信息结构体出现空值缺口。

**Architecture:** 先在 Ed25519 合同测试中增加 `KeyUsage/IsCA` 红测，再对 `GetInfo` 使用 `X509_get_key_usage` 做最小位掩码解码，最后执行证书相关与核心回归。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL X509 API (`X509_get_key_usage`), program-style tests.

---

## Scan Summary (2026-02-12)

### High-signal gap
1. `src/fafafa.ssl.cert.utils.pas` 的 `TCertInfo` 包含 `KeyUsage` 字段，但 `GetInfo` 当前未赋值。
2. 证书生成路径已写入 KeyUsage 扩展（CA: `keyCertSign,cRLSign`; leaf: `digitalSignature,keyEncipherment`），但读取侧丢失该信息。
3. 该缺口会导致上层基于 `GetInfo` 的策略判断缺少关键信号。

### Priority
- **P0:** `GetInfo` KeyUsage metadata completion（test-first）。

---

### Task 1: Add failing KeyUsage contracts (RED)

**Files:**
- Modify: `tests/test_cert_utils_ed25519_contract.pas`

**Step 1: Write failing assertions**
- 在 Ed25519 自签名路径增加断言：`KeyUsage` 非空并包含 `digitalSignature`。
- 在 RSA-CA 路径增加断言：`IsCA=True` 且 `KeyUsage` 包含 `keyCertSign`。
- 在 Ed25519 leaf 路径增加断言：`IsCA=False` 且 `KeyUsage` 包含 `digitalSignature`。

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_cert_utils_ed25519_contract.pas -otmp/test_cert_utils_ed25519_contract`
  - `./tmp/test_cert_utils_ed25519_contract`
- Expected: FAIL（`KeyUsage` 为空）。

---

### Task 2: Implement KeyUsage extraction in GetInfo (GREEN)

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Step 1: Minimal implementation**
- 在 `GetInfo` 中读取 `X509_get_key_usage(LCert)`。
- 将 bitmask 映射为逗号分隔字符串（按固定顺序）：
  - `digitalSignature`, `nonRepudiation`, `keyEncipherment`, `dataEncipherment`, `keyAgreement`, `keyCertSign`, `cRLSign`, `encipherOnly`, `decipherOnly`。
- 若无可用信息保持空串，不改变其他字段语义。

**Step 2: Run test to verify GREEN**
- Run:
  - `fpc -Fu./src tests/test_cert_utils_ed25519_contract.pas -otmp/test_cert_utils_ed25519_contract`
  - `./tmp/test_cert_utils_ed25519_contract`
- Expected: PASS。

---

### Task 3: Focused regression

**Files:**
- Verify only

**Step 1: Certificate enterprise regression**
- Run:
  - `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise`
  - `./tmp/test_x509_enterprise`

**Step 2: Core regressions**
- Run:
  - `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn`
  - `./tmp/test_stream_conn`
  - `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple`
  - `./tmp/run_unit_tests_simple --format=plain --all`
  - `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Acceptance Criteria
- `GetInfo.KeyUsage` 对含 key usage 扩展证书返回非空且包含预期位名。
- Ed25519 自签名与 CA/leaf 合同测试通过。
- 证书与核心回归无新增失败。

## Execution Log (2026-02-12)

### Task 1 RED
- Command:
  - `fpc -Fu./src tests/test_cert_utils_ed25519_contract.pas -otmp/test_cert_utils_ed25519_contract && ./tmp/test_cert_utils_ed25519_contract`
- Result: **FAIL** (expected)
- Key failures:
  - `Key usage should be populated -> FAIL`
  - `CA key usage should include keyCertSign -> FAIL`
  - `Leaf key usage should be populated -> FAIL`

### Task 2 GREEN
- Files modified:
  - `tests/test_cert_utils_ed25519_contract.pas`
  - `src/fafafa.ssl.cert.utils.pas`
- Implementation details:
  - Added KeyUsage/IsCA contract assertions for self-signed, CA, and leaf cert paths.
  - Implemented `GetInfo` KeyUsage bitmask decoding using `X509_get_key_usage` and mapped usage bits to canonical names.
- Command:
  - `fpc -Fu./src tests/test_cert_utils_ed25519_contract.pas -otmp/test_cert_utils_ed25519_contract && ./tmp/test_cert_utils_ed25519_contract`
- Result: **PASS** (`Total tests: 24, Passed: 24, Failed: 0, Skipped: 0`)

### Task 3 Regression
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise` -> PASS (`48/48`)
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn` -> PASS (`10/0/1`)
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all` -> PASS (`N:10 E:0 F:0 I:2`)
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS (`15/15`)
  - Report: `test-reports/test_report_20260212_152746.txt`
