# X509V3 KeyUsage Helper Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 实现 `X509AddKeyUsage` 的真实行为，消除当前占位 `Result := False` 缺口。

**Architecture:** 先添加运行时合同验证“helper 返回成功 + 证书可读出 key usage 位”，再最小实现 helper（文本配置构建扩展并附加），最后跑核心回归。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL X509/X509V3 APIs, program-style tests.

---

### Task 1: Add failing contract (RED)

**Files:**
- Add: `tests/test_x509v3_keyusage_contract.pas`

**Step 1: Write failing test**
- `X509_new()` 创建证书并设置 version=2。
- 调用 `X509AddKeyUsage(Cert, KU_DIGITAL_SIGNATURE or KU_KEY_ENCIPHERMENT)`。
- 断言：
  - helper 返回 `True`
  - `X509_get_key_usage(Cert)` 含 `KU_DIGITAL_SIGNATURE` 与 `KU_KEY_ENCIPHERMENT`。

**Step 2: Run RED**
- `fpc -Fu./src tests/test_x509v3_keyusage_contract.pas -otmp/test_x509v3_keyusage_contract`
- `./tmp/test_x509v3_keyusage_contract`
- Expected: FAIL（当前 helper 占位返回 False）。

---

### Task 2: Minimal implementation (GREEN)

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.x509v3.pas`

**Step 1: Implement helper**
- 将 bitmask 转换为 OpenSSL `keyUsage` 文本值（`digitalSignature,keyEncipherment,...`）。
- 用 `X509V3_EXT_conf_nid(..., NID_key_usage, value)` 构建扩展。
- 通过 `X509_add_ext` 附加扩展并释放 `X509_EXTENSION_free`。
- 仅附加成功时返回 `True`。

**Step 2: Run GREEN**
- `fpc -Fu./src tests/test_x509v3_keyusage_contract.pas -otmp/test_x509v3_keyusage_contract && ./tmp/test_x509v3_keyusage_contract`
- Expected: PASS。

---

### Task 3: Regression

- `fpc -Fu./src tests/test_x509v3_basicconstraints_contract.pas -otmp/test_x509v3_basicconstraints_contract && ./tmp/test_x509v3_basicconstraints_contract`
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract`
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Execution Addendum (2026-02-12, Iteration 94)

### RED
- `fpc -Fu./src tests/test_x509v3_keyusage_contract.pas -otmp/test_x509v3_keyusage_contract && ./tmp/test_x509v3_keyusage_contract`
- Key output:
  - `[FAIL] X509AddKeyUsage should report success`
  - `[FAIL] Extension count should increase by one (before=0, after=0)`
  - `[FAIL] KeyUsage extension should be queryable by NID (index=-1)`

### GREEN
- Modified files:
  - `src/fafafa.ssl.openssl.api.x509v3.pas`
  - `tests/test_x509v3_keyusage_contract.pas`
- Implementation:
  - Implemented `X509AddKeyUsage` bitmask-to-text mapping:
    - `digitalSignature`, `nonRepudiation`, `keyEncipherment`, `dataEncipherment`, `keyAgreement`, `keyCertSign`, `cRLSign`, `encipherOnly`, `decipherOnly`
  - Constructed extension with `X509V3_EXT_conf_nid(..., NID_key_usage, ...)`.
  - Attached extension via `X509_add_ext` and always freed via `X509_EXTENSION_free`.
  - Return `True` only when extension attach succeeds.

### GREEN verify
- `fpc -Fu./src tests/test_x509v3_keyusage_contract.pas -otmp/test_x509v3_keyusage_contract && ./tmp/test_x509v3_keyusage_contract`
- Result: PASS (`Total tests: 5, Passed: 5, Failed: 0`)

### Regression
- `fpc -Fu./src tests/test_x509v3_basicconstraints_contract.pas -otmp/test_x509v3_basicconstraints_contract && ./tmp/test_x509v3_basicconstraints_contract` -> PASS（3/3）
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract` -> PASS（1/1）
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise` -> PASS（48/48）
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn` -> PASS（10/0/1）
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all` -> PASS（N:10 E:0 F:0 I:2）
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS（15/15）
  - Report: `docs/archive/reports/test-report-history/test_report_20260212_164946.txt`
