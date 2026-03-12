# CryptoUtils HKDF Wrapper Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为 `TCryptoUtils` 提供可直接调用的 HKDF 能力（含 Try 版本），并用 RFC5869 向量锁定行为，补齐上层 API 缺口。

**Architecture:** 先新增 RED 合同测试调用不存在的 `TCryptoUtils.HKDF/TryHKDF`；再最小实现包装 `DeriveKeyHKDF`；最后跑 HKDF 与核心回归，确保不影响既有模块。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.crypto.utils`, `fafafa.ssl.openssl.api.kdf`, RFC5869 contract tests.

---

## Scan Summary (2026-02-12)
- 高信号缺口：`tests/benchmarks/benchmark_crypto.pas` 明确标注 “HKDF not yet implemented in TCryptoUtils”。
- 当前仓库已有底层实现：`src/fafafa.ssl.openssl.api.kdf.pas` 的 `DeriveKeyHKDF` 与 RFC5869 测试。
- 缺失点：`src/fafafa.ssl.crypto.utils.pas` 未暴露 HKDF API，导致上层工具类能力不完整。

---

### Task 1 (P0): Add failing HKDF contract test

**Files:**
- Create: `tests/test_crypto_utils_hkdf_contract.pas`

**Step 1: Write failing test**
- 合同覆盖：
  - RFC5869 Case 1（SHA256，42字节输出）结果字节匹配
  - 非法输出长度（0）抛 `ESSLInvalidArgument`
  - `TryHKDF` 对非法长度返回 `False` 且结果为空

**Step 2: Verify RED**
- Run:
  - `fpc -Fu./src tests/test_crypto_utils_hkdf_contract.pas -otmp/test_crypto_utils_hkdf_contract && ./tmp/test_crypto_utils_hkdf_contract`
- Expected:
  - 编译失败（`TCryptoUtils` 尚无 `HKDF/TryHKDF`）

---

### Task 2 (P0): Minimal HKDF wrapper implementation

**Files:**
- Modify: `src/fafafa.ssl.crypto.utils.pas`

**Step 1: Write minimal implementation**
- 在 `TCryptoUtils` 增加：
  - `HKDF(const AKey, ASalt, AInfo: TBytes; AOutputLength: Integer; AAlgorithm: THashAlgorithm = HASH_SHA256): TBytes`
  - `TryHKDF(...; out AResult: TBytes): Boolean`
- 行为约束：
  - `AOutputLength <= 0` -> `RaiseInvalidParameter`
  - 调用 `DeriveKeyHKDF(..., GetEVPDigest(AAlgorithm))`
  - 输出长度必须与 `AOutputLength` 一致，否则抛 `ESSLCryptoError`

**Step 2: Verify GREEN**
- 重跑 Task 1 命令，期望全部 PASS。

---

### Task 3 (P1): Focused regression
- `fpc -Fu./src tests/unit/test_hkdf_rfc5869.pas -otmp/test_hkdf_rfc5869 && ./tmp/test_hkdf_rfc5869`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Acceptance
- `TCryptoUtils` 暴露 HKDF/TryHKDF 上层 API。
- RFC5869 Case1 合同与参数校验合同通过。
- 回归链路维持全绿。

## Execution Record (2026-02-12 14:09 CST)

### RED
- `fpc -Fu./src tests/test_crypto_utils_hkdf_contract.pas -otmp/test_crypto_utils_hkdf_contract && ./tmp/test_crypto_utils_hkdf_contract`
- 关键失败：
  - `Error: Identifier idents no member "HKDF"`
  - `Error: Identifier idents no member "TryHKDF"`

### GREEN
- 修改：
  - `src/fafafa.ssl.crypto.utils.pas`
  - `tests/test_crypto_utils_hkdf_contract.pas`（新）
- 首轮 GREEN 失败（`ESSLCryptoError: HKDF derivation failed`），最小修复：
  - 在 `TCryptoUtils.HKDF` 中显式 `LoadKDFFunctions` + `LoadOpenSSLHMAC`
  - 增加 IKM 长度校验与失败错误收敛
- 最终 GREEN 验证：
  - `✅ HKDF RFC5869 Case 1 verified`
  - `✅ HKDF invalid length contract verified`
  - `✅ All CryptoUtils HKDF contract tests passed`

### Regression
- `fpc -Fu./src tests/unit/test_hkdf_rfc5869.pas -otmp/test_hkdf_rfc5869 && ./tmp/test_hkdf_rfc5869` -> PASS（`Passed:1 Failed:0 Skipped:1`）
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn` -> PASS（`10/0/1`）
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all` -> PASS（`N:10 E:0 F:0 I:2`）
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS（`15/15`, report `docs/archive/reports/test-report-history/test_report_20260212_140825.txt`）
