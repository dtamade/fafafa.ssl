# PKCS11 Structured Error Taxonomy Hardening Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 将 PKCS11 URI/ObjectID/PIN source 路径中残留的 generic exception 统一收敛为 `EPKCS11Exception` + 明确 `CKR_*` 错误码，避免上层错误分类漂移。

**Architecture:** 在 `test_pkcs11_uri_pin_contract` 增加 structured-error 红测（invalid object id / invalid pin-source），再最小修改 `pkcs11.types` helper 函数的异常类型与错误码映射，最后回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.pkcs11.types`, contract-style tests.

---

## Scan Summary (2026-02-12)
- 缺口：`HexToBytesStrict` / `ResolvePINSource` / `ReadPINFromFileStrict` / URI pin-source fallback 仍抛 `Exception`。
- 风险：上层无法按 PKCS11 error taxonomy 做稳定处理（如 `CKR_ARGUMENTS_BAD`、`CKR_PIN_INVALID`）。

---

### Task 1 (P0): Add failing structured-error contracts

**Files:**
- Modify: `tests/test_pkcs11_uri_pin_contract.pas`

**RED assertions:**
1. invalid object id (`id=01GG`) -> `EPKCS11Exception` + `CKR_ARGUMENTS_BAD`
2. invalid pin-source (`prompt:token-ui`) -> `EPKCS11Exception` + `CKR_ARGUMENTS_BAD`

**RED command:**
- `fpc -Fu./src tests/test_pkcs11_uri_pin_contract.pas -otmp/test_pkcs11_uri_pin_contract && ./tmp/test_pkcs11_uri_pin_contract`
- key failure:
  - `Invalid object id should raise EPKCS11Exception, got: Invalid hex character ...`

---

### Task 2 (P0): Minimal implementation

**Files:**
- Modify: `src/fafafa.ssl.pkcs11.types.pas`

**Change set:**
- `HexToBytesStrict` invalid-char/odd-len/encoding -> `EPKCS11Exception(CKR_ARGUMENTS_BAD)`
- `ReadPINFromFileStrict`:
  - empty path -> `CKR_ARGUMENTS_BAD`
  - file missing -> `CKR_GENERAL_ERROR`
  - file empty -> `CKR_PIN_INVALID`
- `ResolvePINSource`:
  - env var name empty -> `CKR_ARGUMENTS_BAD`
  - env var missing/empty -> `CKR_PIN_INVALID`
  - unsupported scheme -> `CKR_ARGUMENTS_BAD`
- `TPKCS11ConfigFromURI` unsupported pin-source scheme -> `CKR_ARGUMENTS_BAD`

**GREEN command:**
- same as RED command
- key success:
  - `✅ Invalid object id structured error verified`
  - `✅ Invalid pin-source structured error verified`

---

### Task 3 (P1): Focused regression
- `fpc -Fu./src tests/pkcs11/test_pkcs11_softhsm.pas -otmp/test_pkcs11_softhsm && ./tmp/test_pkcs11_softhsm`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Execution Record (2026-02-12 13:32 +0800)
- RED fail reproduced ✅
- GREEN pass ✅
- Regression chain pass ✅
  - module batch report: `test-reports/test_report_20260212_133118.txt`
