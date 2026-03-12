# OpenSSL X509 Runtime Export Batch (cmp/get0/policy/revoked) Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 批量补齐 `api.x509` 中“类型已定义但未导出”的关键运行时符号，消除编译缺口并恢复统一 API 契约。

**Architecture:** 先通过批量 RED 合同测试锁定缺口，再在 `src/fafafa.ssl.openssl.api.x509.pas` 一次性补齐 var 导出 + loader 绑定 + unload 清理，最后执行合同链与跨后端/P2 回归验证。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL X509 API, program-style contract tests.

---

### Task 1: Batch RED contract

**Files:**
- Add: `tests/test_x509_runtime_exports_contract.pas`

**Contract symbols:**
- `X509_cmp`
- `X509_get0_notBefore`
- `X509_get0_notAfter`
- `X509_policy_check`
- `X509_policy_tree_free`
- `X509_policy_tree_level_count`
- `X509_policy_tree_get0_level`
- `X509_REVOKED_get0_revocationDate`
- `X509_REVOKED_get_ext_d2i`

**RED command:**
- `fpc -Fu./src tests/test_x509_runtime_exports_contract.pas -otmp/test_x509_runtime_exports_contract`
- `./tmp/test_x509_runtime_exports_contract`
- Expected: 编译失败（符号未导出）。

---

### Task 2: Batch GREEN implementation

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.x509.pas`

**Changes:**
1. var 区新增 9 个 runtime 导出变量。
2. `LoadOpenSSLX509` 中补齐对应 `GetProcedureAddress` 绑定。
3. `UnloadOpenSSLX509` 中补齐 `nil` 清理。

**GREEN command:**
- `fpc -Fu./src tests/test_x509_runtime_exports_contract.pas -otmp/test_x509_runtime_exports_contract && ./tmp/test_x509_runtime_exports_contract`
- Expected: PASS。

---

### Task 3: Regression

- `fpc -Fu./src tests/test_x509_check_email_contract.pas -otmp/test_x509_check_email_contract && ./tmp/test_x509_check_email_contract`
- `fpc -Fu./src tests/test_x509_check_misc_contract.pas -otmp/test_x509_check_misc_contract && ./tmp/test_x509_check_misc_contract`
- `fpc -Fu./src tests/test_x509_check_akid_contract.pas -otmp/test_x509_check_akid_contract && ./tmp/test_x509_check_akid_contract`
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract`
- `fpc -Fu./src tests/test_x509v3_basicconstraints_contract.pas -otmp/test_x509v3_basicconstraints_contract && ./tmp/test_x509v3_basicconstraints_contract`
- `fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_connection && ./tmp/test_stream_connection`
- `fpc -Fu./src tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

Expected: 全部通过。

---

## Execution Record (2026-02-13)

### RED
- Key output:
  - `Identifier not found "X509_cmp"`
  - `Identifier not found "X509_get0_notBefore"`
  - `Identifier not found "X509_get0_notAfter"`
  - `Identifier not found "X509_policy_check"`
  - `Identifier not found "X509_policy_tree_free"`
  - `Identifier not found "X509_policy_tree_level_count"`
  - `Identifier not found "X509_policy_tree_get0_level"`
  - `Identifier not found "X509_REVOKED_get0_revocationDate"`
  - `Identifier not found "X509_REVOKED_get_ext_d2i"`

### GREEN
- Runtime exports added and loaded/unloaded for all 9 symbols above.

### GREEN verify
- Result: `tests/test_x509_runtime_exports_contract.pas` PASS (12/12).

### Regression
- Contract chain: PASS
- `test_backend_contract`: PASS (`Failed: 0`)
- `test_stream_connection`: PASS (`Failed: 0`)
- `run_unit_tests_simple --all`: PASS (`NumberOfFailures=0`)
- P2 modules: PASS (`15/15`)
  - report: `docs/archive/reports/test-report-history/test_report_20260213_002947.txt`
