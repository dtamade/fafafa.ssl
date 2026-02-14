# PKCS11 Interactive PIN Unsupported Taxonomy Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 将 `TPKCS11Config.GetPIN(pmInteractive)` 从通用异常/未实现文案改为结构化 unsupported 语义（错误码 + 文案约束）。

**Architecture:** 先在既有 `test_pkcs11_uri_pin_contract` 增加 RED 断言（异常类型、返回码、文案），再最小修改 `pkcs11.types` 的 `pmInteractive` 分支，最后跑 PKCS11+核心回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.pkcs11.types`, contract-style tests.

---

## Scan Summary (2026-02-12)
- 高信号缺口：`src/fafafa.ssl.pkcs11.types.pas` 中 `pmInteractive` 仍抛 `Exception('Interactive PIN not yet implemented')`。
- 风险：无法进行错误码分层，且沿用 `not implemented` 旧语义，不利于上层 capability/unsupported 分类。

---

### Task 1 (P0): Add failing contract assertion

**Files:**
- Modify: `tests/test_pkcs11_uri_pin_contract.pas`

**Step 1: Add RED contract**
- 新增 `TestConfigInteractivePINUnsupported`：
  - 期望抛出 `EPKCS11Exception`
  - `ReturnValue = CKR_FUNCTION_NOT_SUPPORTED`
  - 消息包含 `unsupported`，不包含 `not implemented`

**Step 2: Verify RED**
- `fpc -Fu./src tests/test_pkcs11_uri_pin_contract.pas -otmp/test_pkcs11_uri_pin_contract && ./tmp/test_pkcs11_uri_pin_contract`
- 期望失败：当前抛的是通用 `Exception`。

---

### Task 2 (P0): Minimal implementation for unsupported taxonomy

**Files:**
- Modify: `src/fafafa.ssl.pkcs11.types.pas`

**Step 1: Minimal implementation**
- `pmInteractive` 分支改为：
  - `raise EPKCS11Exception.Create('Interactive PIN is unsupported in TPKCS11Config.GetPIN; use TPKCS11PINManager', CKR_FUNCTION_NOT_SUPPORTED);`

**Step 2: Verify GREEN**
- 重跑 Task 1 命令，期望全部 PASS。

---

### Task 3 (P1): Focused regression
- `fpc -Fu./src tests/pkcs11/test_pkcs11_softhsm.pas -otmp/test_pkcs11_softhsm && ./tmp/test_pkcs11_softhsm`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Execution Record (2026-02-12 03:26 +0800)

### RED
- `...test_pkcs11_uri_pin_contract...`
- 关键失败：
  - `❌ Interactive PIN path should raise EPKCS11Exception, got: Interactive PIN not yet implemented`

### GREEN
- 修改：`src/fafafa.ssl.pkcs11.types.pas`
- 复跑通过：
  - `✅ Interactive PIN unsupported semantics verified`
  - `✅ All PKCS11 URI/PIN contract tests passed`

### Regression
- `tests/pkcs11/test_pkcs11_softhsm.pas` -> PASS (`通过: 9, 失败: 0, 跳过: 3`)
- `tests/test_stream_connection.pas` -> PASS (`Passed: 10, Failed: 0, Skipped: 1`)
- `tests/unit/run_unit_tests_simple.lpr --all` -> PASS (`N:10 E:0 F:0 I:2`)
- `run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS (`15/15`, report `test_report_20260212_032554.txt`)
