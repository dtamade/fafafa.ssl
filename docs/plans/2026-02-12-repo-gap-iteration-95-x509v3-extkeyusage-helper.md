# X509V3 ExtendedKeyUsage Helper Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 实现 `X509AddExtKeyUsage` 的真实行为，补齐当前占位 `Result := False` 缺口。

**Architecture:** 先写运行时合同（helper success + extension observable + eku bits readable），再最小实现 helper（文本值构建扩展并附加），最后回归。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL X509/X509V3 APIs, program-style tests.

---

### Task 1: Add failing contract (RED)

**Files:**
- Add: `tests/test_x509v3_extkeyusage_contract.pas`

**Step 1: Write failing test**
- `X509_new()` 创建证书并设置 version=2。
- 调用 `X509AddExtKeyUsage(Cert, 'serverAuth,clientAuth')`。
- 断言：
  - helper 返回 `True`
  - 扩展计数 +1
  - `NID_ext_key_usage` 可查询
  - `X509_get_extended_key_usage` 包含 server/client 位

**Step 2: Run RED**
- `fpc -Fu./src tests/test_x509v3_extkeyusage_contract.pas -otmp/test_x509v3_extkeyusage_contract`
- `./tmp/test_x509v3_extkeyusage_contract`
- Expected: FAIL（当前 helper 占位）。

---

### Task 2: Minimal implementation (GREEN)

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.x509v3.pas`

**Step 1: Implement helper**
- 校验输入（`Cert<>nil`、`Usage<>''`）。
- 调用 `X509V3_EXT_conf_nid(..., NID_ext_key_usage, Usage)` 创建扩展。
- 用 `X509_add_ext` 附加，`X509_EXTENSION_free` 释放。
- 仅附加成功返回 `True`。

**Step 2: Run GREEN**
- `fpc -Fu./src tests/test_x509v3_extkeyusage_contract.pas -otmp/test_x509v3_extkeyusage_contract && ./tmp/test_x509v3_extkeyusage_contract`
- Expected: PASS。

---

### Task 3: Regression

- `fpc -Fu./src tests/test_x509v3_keyusage_contract.pas -otmp/test_x509v3_keyusage_contract && ./tmp/test_x509v3_keyusage_contract`
- `fpc -Fu./src tests/test_x509v3_basicconstraints_contract.pas -otmp/test_x509v3_basicconstraints_contract && ./tmp/test_x509v3_basicconstraints_contract`
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract`
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Execution Record (2026-02-12)

### RED
- Command:
  - `fpc -Fu./src tests/test_x509v3_extkeyusage_contract.pas -otmp/test_x509v3_extkeyusage_contract && ./tmp/test_x509v3_extkeyusage_contract`
- Result: FAIL
- Key output:
  - `X509AddExtKeyUsage should report success: FAIL`
  - `Extension count should increase by one: FAIL`
  - `ExtKeyUsage extension should be queryable by NID: FAIL`

### GREEN
- Modified files:
  - `src/fafafa.ssl.openssl.api.x509v3.pas`
  - `tests/test_x509v3_extkeyusage_contract.pas`
- Implementation:
  - Implemented `X509AddExtKeyUsage` with input validation and `Trim`.
  - Created extension via `X509V3_EXT_conf_nid(nil, nil, NID_ext_key_usage, value)`.
  - Attached extension with `X509_add_ext` and guaranteed `X509_EXTENSION_free` in `finally`.
  - Returned `True` only when extension attach succeeded.
- GREEN command:
  - `fpc -Fu./src tests/test_x509v3_extkeyusage_contract.pas -otmp/test_x509v3_extkeyusage_contract && ./tmp/test_x509v3_extkeyusage_contract`
- GREEN result:
  - `Total tests: 5`
  - `Passed: 5`
  - `Failed: 0`
  - `Skipped: 0`

### Regression
- `fpc -Fu./src tests/test_x509v3_keyusage_contract.pas -otmp/test_x509v3_keyusage_contract && ./tmp/test_x509v3_keyusage_contract` -> PASS（5/5）
- `fpc -Fu./src tests/test_x509v3_basicconstraints_contract.pas -otmp/test_x509v3_basicconstraints_contract && ./tmp/test_x509v3_basicconstraints_contract` -> PASS（3/3）
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract` -> PASS（1/1）
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise` -> PASS（48/48）
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn` -> PASS（10/0/1）
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all` -> PASS（N:10 E:0 F:0 I:2）
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS（15/15）
  - Report: `test-reports/test_report_20260212_165435.txt`
