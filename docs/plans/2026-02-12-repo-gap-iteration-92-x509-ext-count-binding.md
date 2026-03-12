# OpenSSL X509 Extension Count Binding Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 补齐 `X509_get_ext_count` 的运行时导出绑定，消除“类型已定义但 API 不可用”的实现缺口。

**Architecture:** 先写 RED 合同测试锁定缺口（符号未导出导致编译失败），再在 `api.x509` 完成变量导出 + 动态加载 + 卸载清理，最后跑核心回归。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL X509 API, program-style tests.

---

### Task 1: Add failing contract (RED)

**Files:**
- Add: `tests/test_x509_ext_count_contract.pas`

**Step 1: Write failing test**
- 加载 OpenSSL Core + X509。
- 调用 `X509_new` 创建证书对象。
- 调用 `X509_get_ext_count` 并断言返回 `0`（新证书无扩展）。

**Step 2: Run RED**
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract`
- `./tmp/test_x509_ext_count_contract`
- Expected: RED（当前 `X509_get_ext_count` 未导出绑定，编译失败）。

---

### Task 2: Minimal implementation (GREEN)

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.x509.pas`

**Step 1: Implement binding export**
- 在 var 区新增：`X509_get_ext_count: TX509_get_ext_count`。
- 在 `LoadOpenSSLX509` 中加载：`GetProcedureAddress(LibHandle, 'X509_get_ext_count')`。
- 在 `UnloadOpenSSLX509` 中置 `nil`。

**Step 2: Run GREEN**
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract`
- Expected: PASS。

---

### Task 3: Focused regression

**Step 1: Enterprise regression**
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise`

**Step 2: Core regression**
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Execution Addendum (2026-02-12, Iteration 92)

### RED
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract`
- Key output:
  - `Identifier not found "X509_get_ext_count"`
- 结论：`TX509_get_ext_count` 类型存在，但 `api.x509` 未导出 runtime 变量绑定。

### GREEN
- Modified files:
  - `src/fafafa.ssl.openssl.api.x509.pas`
  - `tests/test_x509_ext_count_contract.pas`
- Implementation:
  1. 新增 var 导出：`X509_get_ext_count: TX509_get_ext_count`。
  2. 在 `LoadOpenSSLX509` 中绑定 `X509_get_ext_count`。
  3. 在 `UnloadOpenSSLX509` 中置 `nil`。
  4. 新增合同测试验证符号加载（`Assigned(X509_get_ext_count)`）。

### GREEN verify
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract`
- Result: PASS
  - `X509_new symbol should be loaded: PASS`
  - `X509_get_ext_count symbol should be loaded: PASS`

### Regression
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise` -> PASS（48/48）
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn` -> PASS（10/0/1）
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all` -> PASS（N:10 E:0 F:0 I:2）
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS（15/15）
  - Report: `docs/archive/reports/test-report-history/test_report_20260212_162709.txt`

## Correction Addendum (2026-02-12, post-validation)

- 复盘发现先前 runtime AV 复现同样受 `X509_new` 漏写 `()` 影响（函数指针被当作证书指针）。
- 在修正调用后，`X509_get_ext_count` 运行时合同可稳定通过：
  - `Newly created X509 should have zero extensions: PASS`
- Iteration 92 结论升级为：
  1. 绑定缺口已补齐（编译面）。
  2. 运行时调用合同也可通过（执行面）。
