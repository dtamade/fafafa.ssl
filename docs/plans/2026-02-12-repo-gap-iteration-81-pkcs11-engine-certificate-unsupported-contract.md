# PKCS11 ENGINE Certificate Unsupported Contract Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 ENGINE 后端 `LoadCertificate` 路径为确定性 unsupported 语义，避免在不支持环境触发访问违规或误导性错误。

**Architecture:** 在 `test_pkcs11_uri_pin_contract` 增加 `TEngineBackend.LoadCertificate` 的 RED 合同，随后最小改动 `pkcs11.engine`：保留参数校验，直接返回 `CKR_FUNCTION_NOT_SUPPORTED`，不再进入引擎加载副作用路径。最后执行 PKCS11 + 核心回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.pkcs11.engine`, contract-style tests.

---

## Scan Summary (2026-02-12)
- 高信号缺口：`src/fafafa.ssl.pkcs11.engine.pas` 的 `LoadCertificate` 虽标注 unsupported，但实际先走 `LoadEngine/ResolvePIN/PIN ctrl`，在当前环境出现 `Access violation`。
- 目标：把行为固定为“输入校验后立即 unsupported”，避免环境依赖导致的非合同失败。

---

### Task 1 (P0): Add failing contract

**Files:**
- Modify: `tests/test_pkcs11_uri_pin_contract.pas`

**RED assertion:**
- `TEngineBackend.LoadCertificate` should raise:
  - `EPKCS11Exception`
  - `ReturnValue = CKR_FUNCTION_NOT_SUPPORTED`
  - message contains `unsupported`
  - message excludes `not yet implemented`

**RED command:**
- `fpc -Fu./src tests/test_pkcs11_uri_pin_contract.pas -otmp/test_pkcs11_uri_pin_contract && ./tmp/test_pkcs11_uri_pin_contract`

---

### Task 2 (P0): Minimal implementation

**Files:**
- Modify: `src/fafafa.ssl.pkcs11.engine.pas`

**Change:**
- `LoadCertificate`:
  1. 保留 `ValidateConfig(AConfig)`
  2. 直接抛出 `EPKCS11Exception(...CKR_FUNCTION_NOT_SUPPORTED)`
  3. message 使用 `unsupported`，不再出现 `not yet implemented`

**GREEN command:**
- same as RED command

---

### Task 3 (P1): Focused regression
- `fpc -Fu./src tests/pkcs11/test_pkcs11_softhsm.pas -otmp/test_pkcs11_softhsm && ./tmp/test_pkcs11_softhsm`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Execution Record (2026-02-12 03:29 +0800)

### RED
- key failure:
  - `❌ ENGINE certificate load should raise EPKCS11Exception, got: Access violation`

### GREEN
- modified `src/fafafa.ssl.pkcs11.engine.pas`
- key success:
  - `✅ ENGINE certificate unsupported contract verified`
  - `✅ All PKCS11 URI/PIN contract tests passed`

### Regression
- `tests/pkcs11/test_pkcs11_softhsm.pas` -> PASS (`通过: 9, 失败: 0, 跳过: 3`)
- `tests/test_stream_connection.pas` -> PASS (`Passed: 10, Failed: 0, Skipped: 1`)
- `tests/unit/run_unit_tests_simple.lpr --all` -> PASS (`N:10 E:0 F:0 I:2`)
- `run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS (`15/15`, report `test_report_20260212_032833.txt`)
