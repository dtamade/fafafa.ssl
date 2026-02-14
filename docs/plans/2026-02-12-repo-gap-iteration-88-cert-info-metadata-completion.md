# Certificate Info Metadata Completion Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 补齐 `TCertificateUtils.GetInfo` 的核心元数据提取（序列号、签名算法、公钥类型/位数），避免返回空字段导致上层契约失真。

**Architecture:** 先在 Ed25519 合同测试中新增元数据断言并制造 RED，再最小修改 `GetInfo` 增加 ASN.1/X509 提取，最后跑证书与核心回归。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL X509/ASN1 APIs, `fafafa.ssl.cert.utils`.

---

## Scan Summary (2026-02-12)
- 高信号缺口：`GetInfo` 仅返回部分字段，`SerialNumber` 和 `SignatureAlgorithm` 为空。
- 附带问题：`NotBefore` 赋值重复一行。

---

### Task 1 (P0): Add failing metadata contracts

**Files:**
- Modify: `tests/test_cert_utils_ed25519_contract.pas`

**Step 1: RED assertions**
- 在 self-signed 与 CA-signed 两条路径增加断言：
  - `SerialNumber <> ''`
  - `SignatureAlgorithm <> ''`
  - `PublicKeyBits > 0`

**Step 2: Verify RED**
- `fpc -Fu./src tests/test_cert_utils_ed25519_contract.pas -otmp/test_cert_utils_ed25519_contract && ./tmp/test_cert_utils_ed25519_contract`
- 期望：`SerialNumber` 与 `SignatureAlgorithm` 断言失败。

---

### Task 2 (P0): Minimal metadata extraction

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Step 1: Minimal implementation**
- `GetInfo` 中补齐：
  - ASN.1 serial 提取（`ASN1_INTEGER_get_int64` / `ASN1_INTEGER_get`）
  - 签名算法 NID -> 名称（`X509_get_signature_nid + OBJ_nid2sn`）
  - 保持公钥类型/位数提取
  - 移除重复 `NotBefore` 赋值

**Step 2: Verify GREEN**
- 重跑 RED 命令，期望全部通过。

---

### Task 3 (P1): Regression
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Execution Record (2026-02-12 14:42 CST)

### RED
- `fpc -Fu./src tests/test_cert_utils_ed25519_contract.pas -otmp/test_cert_utils_ed25519_contract && ./tmp/test_cert_utils_ed25519_contract`
- 关键失败：
  - `Serial number should be populated -> FAIL`
  - `Signature algorithm should be populated -> FAIL`
  - `Leaf serial number should be populated -> FAIL`
  - `Leaf signature algorithm should be populated -> FAIL`

### GREEN
- 修改：`src/fafafa.ssl.cert.utils.pas`
  - `GetInfo` 新增 serial/signature 抽取
  - 修复 `NotBefore` 重复赋值
- 复跑通过：
  - `Total tests: 16, Passed: 16, Failed: 0, Skipped: 0`

### Regression
- `test_x509_enterprise` -> PASS（`48/48`）
- `test_stream_connection` -> PASS（`10/0/1`）
- `run_unit_tests_simple --all` -> PASS（`N:10 E:0 F:0 I:2`）
- `run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS（`15/15`, report `test-reports/test_report_20260212_144138.txt`）
