# OpenSSL X509 Check AKID Binding Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 补齐 `X509_check_akid` 的运行时导出绑定，消除“类型已定义但 API 不可用”的实现缺口。

**Architecture:** 先以 RED 合同测试锁定符号缺口（编译期未导出），再在 `api.x509` 完成 runtime var 导出 + loader 绑定 + unload 清理三点最小修复，最后执行全链回归确认无副作用。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL X509 API, program-style contract tests.

---

### Task 1: Add failing contract (RED)

**Files:**
- Add: `tests/test_x509_check_akid_contract.pas`

**Step 1: Write failing test**
- 加载 `OpenSSL Core + X509`。
- 断言 `Assigned(X509_check_akid)`。

**Step 2: Run RED**
- `fpc -Fu./src tests/test_x509_check_akid_contract.pas -otmp/test_x509_check_akid_contract`
- `./tmp/test_x509_check_akid_contract`
- Expected: 编译失败（当前 `X509_check_akid` 未导出）。

---

### Task 2: Minimal implementation (GREEN)

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.x509.pas`

**Step 1: Implement binding export**
- 在 var 区新增：`X509_check_akid: TX509_check_akid`。
- 在 `LoadOpenSSLX509` 中加载：`GetProcedureAddress(LibHandle, 'X509_check_akid')`。
- 在 `UnloadOpenSSLX509` 中置 `nil`。

**Step 2: Run GREEN**
- `fpc -Fu./src tests/test_x509_check_akid_contract.pas -otmp/test_x509_check_akid_contract && ./tmp/test_x509_check_akid_contract`
- Expected: PASS。

---

### Task 3: Regression

- `fpc -Fu./src tests/test_x509_check_email_contract.pas -otmp/test_x509_check_email_contract && ./tmp/test_x509_check_email_contract`
- `fpc -Fu./src tests/test_x509_check_misc_contract.pas -otmp/test_x509_check_misc_contract && ./tmp/test_x509_check_misc_contract`
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract`
- `fpc -Fu./src tests/test_x509v3_basicconstraints_contract.pas -otmp/test_x509v3_basicconstraints_contract && ./tmp/test_x509v3_basicconstraints_contract`
- `fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_connection && ./tmp/test_stream_connection`
- `fpc -Fu./src tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

Expected: 全部通过。

---

## Execution Record (2026-02-12)

### RED
- Command:
  - `fpc -Fu./src tests/test_x509_check_akid_contract.pas -otmp/test_x509_check_akid_contract && ./tmp/test_x509_check_akid_contract`
- Key output:
  - `Identifier not found "X509_check_akid"`

### GREEN
- Modified file:
  - `src/fafafa.ssl.openssl.api.x509.pas`
- Implementation:
  1. 新增 runtime 导出：`X509_check_akid: TX509_check_akid`。
  2. 在 `LoadOpenSSLX509` 中绑定 `X509_check_akid`。
  3. 在 `UnloadOpenSSLX509` 中 `X509_check_akid := nil`。

### GREEN verify
- Command:
  - `fpc -Fu./src tests/test_x509_check_akid_contract.pas -otmp/test_x509_check_akid_contract && ./tmp/test_x509_check_akid_contract`
- Result: PASS
  - `[PASS] X509_check_akid symbol should be loaded`

### Regression
- Contract chain (email/misc/ext_count/basicconstraints): PASS
- `test_backend_contract`: PASS (`Failed: 0`)
- `test_stream_connection`: PASS (`Failed: 0`)
- `run_unit_tests_simple --all`: PASS (`NumberOfFailures=0`)
- P2 modules: PASS (`15/15`)
  - report: `docs/archive/reports/test-report-history/test_report_20260212_230410.txt`
