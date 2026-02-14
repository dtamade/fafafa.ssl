# OpenSSL X509 Purpose/Trust/Issued Binding Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 补齐 `X509_check_purpose` / `X509_check_trust` / `X509_check_issued` 的运行时导出绑定，消除“类型已定义但 API 不可用”的实现缺口。

**Architecture:** 先新增 RED 合同测试锁定编译缺口（标识符未导出），再在 `api.x509` 完成 var 导出 + loader 绑定 + unload 清理，最后执行 x509/后端/P2 聚焦回归，确认无行为回退。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL X509 API, program-style contract tests.

---

### Task 1: Add failing contract (RED)

**Files:**
- Add: `tests/test_x509_check_misc_contract.pas`

**Step 1: Write failing test**
- 加载 `OpenSSL Core + X509`。
- 断言以下符号可用：
  - `X509_check_purpose`
  - `X509_check_trust`
  - `X509_check_issued`
- 使用空证书对象调用三个函数，验证可调用（不崩溃且返回有效编码）。

**Step 2: Run RED**
- `fpc -Fu./src tests/test_x509_check_misc_contract.pas -otmp/test_x509_check_misc_contract`
- `./tmp/test_x509_check_misc_contract`
- Expected: 编译失败（当前符号未导出）。

---

### Task 2: Minimal implementation (GREEN)

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.x509.pas`

**Step 1: Implement binding export**
- 在 var 区新增：
  - `X509_check_purpose: TX509_check_purpose`
  - `X509_check_trust: TX509_check_trust`
  - `X509_check_issued: TX509_check_issued`
- 在 `LoadOpenSSLX509` 中绑定对应 `GetProcedureAddress`。
- 在 `UnloadOpenSSLX509` 中置 `nil`。

**Step 2: Run GREEN**
- `fpc -Fu./src tests/test_x509_check_misc_contract.pas -otmp/test_x509_check_misc_contract && ./tmp/test_x509_check_misc_contract`
- Expected: PASS。

---

### Task 3: Focused regression

**Step 1: X509 contracts**
- `fpc -Fu./src tests/test_x509_check_email_contract.pas -otmp/test_x509_check_email_contract && ./tmp/test_x509_check_email_contract`
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract`
- `fpc -Fu./src tests/test_x509v3_basicconstraints_contract.pas -otmp/test_x509v3_basicconstraints_contract && ./tmp/test_x509v3_basicconstraints_contract`
- `fpc -Fu./src tests/test_x509v3_keyusage_contract.pas -otmp/test_x509v3_keyusage_contract && ./tmp/test_x509v3_keyusage_contract`
- `fpc -Fu./src tests/test_x509v3_extkeyusage_contract.pas -otmp/test_x509v3_extkeyusage_contract && ./tmp/test_x509v3_extkeyusage_contract`

**Step 2: Cross-backend and module regression**
- `fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_connection && ./tmp/test_stream_connection`
- `fpc -Fu./src tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

Expected: 全部通过。

---

## Execution Record (2026-02-12)

### RED
- Command:
  - `fpc -Fu./src tests/test_x509_check_misc_contract.pas -otmp/test_x509_check_misc_contract && ./tmp/test_x509_check_misc_contract`
- Key output:
  - `Identifier not found "X509_check_purpose"`
  - `Identifier not found "X509_check_trust"`
  - `Identifier not found "X509_check_issued"`

### GREEN
- Modified file:
  - `src/fafafa.ssl.openssl.api.x509.pas`
- Implementation:
  1. 新增三个 runtime 变量导出（purpose/trust/issued）。
  2. 在 `LoadOpenSSLX509` 中绑定三个符号。
  3. 在 `UnloadOpenSSLX509` 中三个符号置 `nil`。

### GREEN verify
- Command:
  - `fpc -Fu./src tests/test_x509_check_misc_contract.pas -otmp/test_x509_check_misc_contract && ./tmp/test_x509_check_misc_contract`
- Result: PASS（6/6）

### Regression
- X509/x509v3 contract chain: PASS
- `tests/contract/test_backend_contract.pas`: PASS（Failed=0）
- `tests/test_stream_connection.pas`: PASS（Failed=0）
- `tests/unit/run_unit_tests_simple.lpr --all`: PASS（NumberOfFailures=0）
- P2 module suite: PASS（15/15）
  - report: `test-reports/test_report_20260212_204309.txt`
