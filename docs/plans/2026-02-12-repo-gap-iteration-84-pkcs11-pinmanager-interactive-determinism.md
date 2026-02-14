# PKCS11 PIN Manager Interactive Determinism Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 `TPKCS11PINManager.GetPIN(pmInteractive)` 在非交互场景下行为确定化，避免阻塞/挂起，并统一为结构化 unsupported 语义。

**Architecture:** 先在 `test_pkcs11_uri_pin_contract` 增加 RED 契约（异常类型、错误码、文案约束），再最小修改 `pkcs11.pin` 的 `pmInteractive` 分支为 deterministic unsupported，最后执行 PKCS11 + TS + 核心回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.pkcs11.pin`, contract-style tests.

---

## Scan Summary (2026-02-12)
- 高信号缺口：`src/fafafa.ssl.pkcs11.pin.pas` 交互读取路径仍为 `ReadLn` 简化实现，在自动化/无控制台环境存在阻塞风险，且错误分类不稳定。
- 目标语义：此层对 `pmInteractive` 统一返回 `EPKCS11Exception(CKR_FUNCTION_NOT_SUPPORTED)`，消息包含 `unsupported`，不包含 `not implemented`。

---

### Task 1 (P0): Add failing contract assertion

**Files:**
- Modify: `tests/test_pkcs11_uri_pin_contract.pas`

**Step 1: Write failing test**
- 新增 `TestPINManagerInteractiveUnsupportedContract`：
  - 调用 `TPKCS11PINManager.GetPIN(pmInteractive, '', nil, 'TestToken')`
  - 断言抛出 `EPKCS11Exception`
  - `ReturnValue = CKR_FUNCTION_NOT_SUPPORTED`
  - 消息包含 `unsupported`，且不包含 `not implemented`

**Step 2: Verify RED**
- Run:
  - `fpc -Fu./src tests/test_pkcs11_uri_pin_contract.pas -otmp/test_pkcs11_uri_pin_contract && ./tmp/test_pkcs11_uri_pin_contract`
- Expected:
  - FAIL（当前实现会走交互读取，异常类型/错误码不满足契约）

---

### Task 2 (P0): Minimal implementation for deterministic interactive path

**Files:**
- Modify: `src/fafafa.ssl.pkcs11.pin.pas`

**Step 1: Write minimal implementation**
- 在 `TPKCS11PINManager.GetPIN` 的 `pmInteractive` 分支改为结构化 unsupported：
  - `raise EPKCS11Exception.Create('Interactive PIN is unsupported in TPKCS11PINManager.GetPIN; use callback/value/file/environment methods', CKR_FUNCTION_NOT_SUPPORTED);`

**Step 2: Verify GREEN**
- 重跑 Task 1 命令，期望全部 PASS。

---

### Task 3 (P1): Focused regression
- `fpc -Fu./src tests/pkcs11/test_pkcs11_softhsm.pas -otmp/test_pkcs11_softhsm && ./tmp/test_pkcs11_softhsm`
- `fpc -Fu./src tests/certificate/test_tsa_api.pas -otmp/test_tsa_api && ./tmp/test_tsa_api`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Acceptance
- `test_pkcs11_uri_pin_contract` 新增 interactive contract 通过。
- `pmInteractive` 不再尝试控制台读取，不再出现非确定性行为。
- 关键回归链路保持全绿。

## Execution Record (2026-02-12 13:58 CST)

### RED
- `fpc -Fu./src tests/test_pkcs11_uri_pin_contract.pas -otmp/test_pkcs11_uri_pin_contract && ./tmp/test_pkcs11_uri_pin_contract < /dev/null`
- 关键失败：
  - `❌ PIN manager interactive path should raise CKR_FUNCTION_NOT_SUPPORTED (expected=84 actual=161)`

### GREEN
- 修改：`src/fafafa.ssl.pkcs11.pin.pas`
  - `TPKCS11PINManager.GetPIN(pmInteractive)` 改为 deterministic `EPKCS11Exception(CKR_FUNCTION_NOT_SUPPORTED)`。
- 复跑通过：
  - `✅ PIN manager interactive unsupported contract verified`
  - `✅ All PKCS11 URI/PIN contract tests passed`

### Regression
- `fpc -Fu./src tests/pkcs11/test_pkcs11_softhsm.pas -otmp/test_pkcs11_softhsm && ./tmp/test_pkcs11_softhsm` -> PASS (`通过: 9, 失败: 0, 跳过: 3`)
- `fpc -Fu./src tests/certificate/test_tsa_api.pas -otmp/test_tsa_api && ./tmp/test_tsa_api` -> PASS (`20/20`)
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn` -> PASS (`Passed: 10, Failed: 0, Skipped: 1`)
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all` -> PASS (`N:10 E:0 F:0 I:2`)
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS (`15/15`, report `test-reports/test_report_20260212_135750.txt`)
