# OpenSSL X509 Check Email Binding Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 补齐 `X509_check_email` 的运行时导出绑定，消除“类型已定义但 API 不可用”的实现缺口。

**Architecture:** 先通过 RED 合同测试锁定符号缺口（编译期未导出），再在 `api.x509` 中完成变量导出 + 动态加载 + 卸载清理三点最小修复，最后执行聚焦回归与模块回归保证无旁路影响。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL X509 API, program-style contract tests.

---

### Task 1: Add failing contract (RED)

**Files:**
- Add/Modify: `tests/test_x509_check_email_contract.pas`

**Step 1: Write failing test**
- 加载 `OpenSSL Core + X509`。
- 断言 `Assigned(X509_check_email)`。
- 对新建空证书调用 `X509_check_email`，验证可调用且不会误报匹配。

**Step 2: Run RED**
- `fpc -Fu./src tests/test_x509_check_email_contract.pas -otmp/test_x509_check_email_contract`
- `./tmp/test_x509_check_email_contract`
- Expected: 编译失败（当前 `X509_check_email` 未导出）。

---

### Task 2: Minimal implementation (GREEN)

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.x509.pas`

**Step 1: Implement binding export**
- 在 var 区新增：`X509_check_email: TX509_check_email`。
- 在 `LoadOpenSSLX509` 中加载：`GetProcedureAddress(LibHandle, 'X509_check_email')`。
- 在 `UnloadOpenSSLX509` 中置 `nil`。

**Step 2: Run GREEN**
- `fpc -Fu./src tests/test_x509_check_email_contract.pas -otmp/test_x509_check_email_contract && ./tmp/test_x509_check_email_contract`
- Expected: PASS。

---

### Task 3: Focused regression

**Files:**
- Reuse existing tests only

**Step 1: X509/X509v3 contracts**
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract`
- `fpc -Fu./src tests/test_x509v3_basicconstraints_contract.pas -otmp/test_x509v3_basicconstraints_contract && ./tmp/test_x509v3_basicconstraints_contract`
- `fpc -Fu./src tests/test_x509v3_basicconstraints_pathlen_contract.pas -otmp/test_x509v3_basicconstraints_pathlen_contract && ./tmp/test_x509v3_basicconstraints_pathlen_contract`
- `fpc -Fu./src tests/test_x509v3_keyusage_contract.pas -otmp/test_x509v3_keyusage_contract && ./tmp/test_x509v3_keyusage_contract`
- `fpc -Fu./src tests/test_x509v3_extkeyusage_contract.pas -otmp/test_x509v3_extkeyusage_contract && ./tmp/test_x509v3_extkeyusage_contract`
- `fpc -Fu./src tests/test_x509v3_subjectaltname_contract.pas -otmp/test_x509v3_subjectaltname_contract && ./tmp/test_x509v3_subjectaltname_contract`

**Step 2: Backend and module regression**
- `fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_connection && ./tmp/test_stream_connection`
- `fpc -Fu./src tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

Expected: 全部通过。

---

## Execution Record (2026-02-12)

### RED
- Command:
  - `fpc -Fu./src tests/test_x509_check_email_contract.pas -otmp/test_x509_check_email_contract && ./tmp/test_x509_check_email_contract`
- Key output:
  - `Identifier not found "X509_check_email"` (3 occurrences)

### GREEN
- Modified file:
  - `src/fafafa.ssl.openssl.api.x509.pas`
- Implementation:
  1. 新增 runtime 导出：`X509_check_email: TX509_check_email`。
  2. 在 `LoadOpenSSLX509` 中绑定 `X509_check_email`。
  3. 在 `UnloadOpenSSLX509` 中将 `X509_check_email := nil`。

### GREEN Verify
- Command:
  - `fpc -Fu./src tests/test_x509_check_email_contract.pas -otmp/test_x509_check_email_contract && ./tmp/test_x509_check_email_contract`
- Result: PASS
  - `[PASS] X509_check_email symbol should be loaded`
  - `[PASS] X509_check_email should be callable and not match empty cert`

### Regression
- X509/X509v3 contracts: PASS
- `tests/contract/test_backend_contract.pas`: PASS（`Failed: 0`）
- `tests/test_stream_connection.pas`: PASS（`Failed: 0`）
- `tests/unit/run_unit_tests_simple.lpr --all`: PASS（`NumberOfFailures=0`）
- P2 module suite: PASS（`15/15`, report: `test-reports/test_report_20260212_194600.txt`）
