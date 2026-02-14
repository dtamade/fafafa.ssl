# PKCS11 URI/PIN Hardening Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 修复 PKCS11 URI/ObjectID/PIN 路径中的未完成/宽松行为，避免 key id 丢失和 PIN 源解析歧义。

**Architecture:** 先用独立契约测试程序建立 RED（ObjectID 十六进制转换、pin-source=file、callback 空 PIN 拒绝），再在 `pkcs11.types` 与 `pkcs11.pin` 做最小实现。最后执行 PKCS11 + 核心回归链，确保无回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.pkcs11.*`, program-style tests.

---

## Scan Summary (2026-02-12)

### High-signal gap
1. `src/fafafa.ssl.pkcs11.types.pas`
   - `TPKCS11ConfigFromURI` 未将 URI `id=` 转换为 `KeyID` 字节数组（留空）。
   - `TPKCS11URI.GetPIN` 在 `pin-source` 路径直接抛 `not yet implemented`。
   - `TPKCS11Config.GetPIN` 的 `pmFile` 路径吞异常返回空字符串，诊断不明确。
2. `src/fafafa.ssl.pkcs11.pin.pas`
   - `pmCallback` 在 callback 返回 `True` 且 PIN 为空时未拒绝，形成空 PIN 假阳性。

### Priority
- **P0:** PKCS11 URI/PIN 合同硬化（TDD）。

---

### Task 1 (P0): Add failing contract test

**Files:**
- Create: `tests/test_pkcs11_uri_pin_contract.pas`

**Step 1: Write failing assertions**
- `ObjectID` hex -> `KeyID` bytes conversion contract.
- `pin-source=file` contract (`URI.GetPIN` should return trimmed PIN).
- callback-empty-PIN reject contract (`CKR_PIN_INVALID`).

**Step 2: Verify RED**
- Run:
  - `fpc -Fu./src tests/test_pkcs11_uri_pin_contract.pas -otmp/test_pkcs11_uri_pin_contract && ./tmp/test_pkcs11_uri_pin_contract`
- Expected:
  - FAIL at `Object ID hex should be converted into key bytes`.

---

### Task 2 (P0): Minimal implementation for URI/PIN hardening

**Files:**
- Modify: `src/fafafa.ssl.pkcs11.types.pas`
- Modify: `src/fafafa.ssl.pkcs11.pin.pas`

**Step 1: Minimal implementation**
- 在 `pkcs11.types` 增加：
  - `HexToBytesStrict`：将 URI `id=` 十六进制字符串转换为 `TBytes`。
  - `ResolvePINSource` + `ReadPINFromFileStrict`：支持 `env:` / `file:` 解析并给出明确异常。
- `TPKCS11URI.GetPIN` 改为调用 `ResolvePINSource`。
- `TPKCS11ConfigFromURI` 填充 `Result.KeyID`。
- `TPKCS11Config.GetPIN(pmFile)` 改为 strict file 读取，不吞异常。
- 在 `pkcs11.pin` 的 `pmCallback` 分支增加 empty PIN 拒绝。

**Step 2: Verify GREEN**
- Run:
  - `fpc -Fu./src tests/test_pkcs11_uri_pin_contract.pas -otmp/test_pkcs11_uri_pin_contract && ./tmp/test_pkcs11_uri_pin_contract`
- Expected:
  - PASS with all three contracts green.

---

### Task 3 (P1): Focused regression

**Step 1: PKCS11 regression**
- `fpc -Fu./src tests/pkcs11/test_pkcs11_softhsm.pas -otmp/test_pkcs11_softhsm && ./tmp/test_pkcs11_softhsm`

**Step 2: Core regression**
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Execution Notes
- 严格 TDD：RED -> GREEN -> Regression。
- 不新增脚本、不改 CI/DI。
- 每一步命令输出记录在 `progress.md`。

---

## Execution Record (2026-02-12 03:22 +0800)

### Task 1 (RED)
- Command:
  - `fpc -Fu./src tests/test_pkcs11_uri_pin_contract.pas -otmp/test_pkcs11_uri_pin_contract && ./tmp/test_pkcs11_uri_pin_contract`
- Key output:
  - `❌ Object ID hex should be converted into key bytes (expected=3 actual=0)`

### Task 2 (GREEN)
- Modified:
  - `src/fafafa.ssl.pkcs11.types.pas`
  - `src/fafafa.ssl.pkcs11.pin.pas`
- Re-run command:
  - `fpc -Fu./src tests/test_pkcs11_uri_pin_contract.pas -otmp/test_pkcs11_uri_pin_contract && ./tmp/test_pkcs11_uri_pin_contract`
- Key output:
  - `✅ Object ID conversion contract verified`
  - `✅ PIN source file contract verified`
  - `✅ Callback PIN contract verified`

### Task 3 (Regression)
- `tests/pkcs11/test_pkcs11_softhsm.pas` -> PASS (`通过: 9, 失败: 0, 跳过: 3`)
- `tests/test_stream_connection.pas` -> PASS (`Passed: 10, Failed: 0, Skipped: 1`)
- `tests/unit/run_unit_tests_simple.lpr --all` -> PASS (`N:10 E:0 F:0 I:2`)
- `run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS (`15/15`)
