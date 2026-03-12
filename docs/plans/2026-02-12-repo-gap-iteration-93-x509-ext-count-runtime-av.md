# OpenSSL X509 Extension Count Runtime AV Fix Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 修复 `X509_get_ext_count` 在 core-loader 路径下的运行时访问违规（AV），确保 `X509_new` 产出的证书对象可安全调用扩展计数 API。

**Architecture:** 先构建 RED 运行时合同（`X509_new -> X509_get_ext_count`），再定位并修复加载/调用链根因，最后跑全链回归。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL core/x509 APIs, program-style tests.

---

### Task 1: Reproduce RED with runtime contract

**Files:**
- Add/Modify: `tests/test_x509_ext_count_runtime_contract.pas`

**Step 1: Write failing runtime assertion**
- `LoadOpenSSLCore + LoadOpenSSLX509`
- `X509_new`
- 调用 `X509_get_ext_count` 期望返回 `0` 且不抛异常。

**Step 2: Run RED**
- `fpc -Fu./src tests/test_x509_ext_count_runtime_contract.pas -otmp/test_x509_ext_count_runtime_contract`
- `./tmp/test_x509_ext_count_runtime_contract`
- Expected: FAIL（当前 AV）。

---

### Task 2: Minimal implementation

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.core.pas` and/or `src/fafafa.ssl.openssl.api.x509.pas`

**Step 1: Diagnose loader/runtime path**
- 验证 core loader 句柄与 direct load 句柄差异。
- 定位是否为初始化调用、副作用或签名不匹配引发内存破坏。

**Step 2: Implement minimal safe fix**
- 仅做最小改动，确保 `X509_new -> X509_get_ext_count` 在 core-loader 下可调用且返回稳定值。

**Step 3: Run GREEN**
- 重跑 Task 1 命令，期望 PASS。

---

### Task 3: Regression

- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract`
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Execution Addendum (2026-02-12, Iteration 93)

### RED root-cause定位
- 通过最小复现程序排查发现，AV 的直接触发点并非 OpenSSL loader，而是测试调用写法：
  - 错误：`LCert := X509_new;`
  - 正确：`LCert := X509_new();`
- 错误写法会把函数地址误作为 `PX509` 传给 `X509_get_ext_count` / `X509_free`，导致访问违规。

### GREEN 实施
- `tests/test_x509v3_basicconstraints_contract.pas`
  - 修正 `X509_new()` 调用并恢复强合同（success + ext count + NID 命中）。
- `tests/test_x509_ext_count_contract.pas`
  - 升级为运行时合同：`X509_new() -> X509_get_ext_count = 0`。
- `src/fafafa.ssl.openssl.api.x509v3.pas`
  - 恢复 `X509AddBasicConstraints` 的真实实现（`X509V3_EXT_i2d + X509_add_ext`）。

### GREEN 验证
- `fpc -Fu./src tests/test_x509v3_basicconstraints_contract.pas -otmp/test_x509v3_basicconstraints_contract && ./tmp/test_x509v3_basicconstraints_contract`
  - `3/3 PASS`
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract`
  - `1/1 PASS`

### Regression
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise` -> PASS（48/48）
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn` -> PASS（10/0/1）
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all` -> PASS（N:10 E:0 F:0 I:2）
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS（15/15）
  - Report: `docs/archive/reports/test-report-history/test_report_20260212_163708.txt`
