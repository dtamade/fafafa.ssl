# Capability Serializer Strict Roundtrip Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 修复 `TSSLBackendCapabilities` 在 JSON/XML 序列化与反序列化中的字段不对称，确保跨后端能力信息可严格 roundtrip。

**Architecture:** 先在 `test_capability_deserialization_roundtrip` 增加缺口字段断言制造 RED；再以最小实现补齐 `src/fafafa.ssl.capability.serializer.pas` 的 JSON/XML 双向映射（支持级别字段、算法集合字段、证书/密钥与扩展能力字段）；最后执行聚焦回归与稳定性 gate。

**Tech Stack:** FreePascal (ObjFPC), `src/fafafa.ssl.capability.serializer.pas`, `tests/test_capability_deserialization_roundtrip.pas`.

---

### Task 1 (P0): Add failing strict roundtrip assertions

**Files:**
- Modify: `tests/test_capability_deserialization_roundtrip.pas`
- Target: `src/fafafa.ssl.capability.serializer.pas`

**Step 1: Write failing assertions**
- 在 `AssertRoundTripEqual` 增加以下字段对比：
  - Support level: `SessionCacheSupport`, `ZeroRTTSupport`, `EarlyDataSupport`, `RenegotiationSupport`, `PostHandshakeAuthSupport`
  - Algorithm sets: `SupportedCiphers`, `SupportedHashes`, `SupportedKeyExchanges`
  - Key/cert bools: `SupportsDERPrivateKey`, `SupportsPKCS8PrivateKey`, `SupportsPKCS12`, `SupportsPasswordProtectedKeys`
  - Extensibility bools: `SupportsCustomCipherSuites`, `SupportsCallbacks`

**Step 2: Run RED command**
- `fpc -Fu./src tests/test_capability_deserialization_roundtrip.pas -otmp/test_cap_roundtrip && ./tmp/test_cap_roundtrip`
- Expected: FAIL（新增字段在 serializer 中尚未完整映射）。

---

### Task 2 (P0): Implement serializer/deserializer symmetry

**Files:**
- Modify: `src/fafafa.ssl.capability.serializer.pas`

**Step 1: Minimal implementation**
- JSON/XML 序列化新增上述字段。
- JSON/XML 反序列化新增对应解析。
- 为 set 字段增加稳定字符串编码/解码（保证 roundtrip 对称）。

**Step 2: Run GREEN command**
- `fpc -Fu./src tests/test_capability_deserialization_roundtrip.pas -otmp/test_cap_roundtrip && ./tmp/test_cap_roundtrip`
- Expected: PASS。

---

### Task 3 (P0/P1): Focused regression + stability gate

**Step 1: Focused regression**
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept`

**Step 2: P0-18 unit subset gate**
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`

**Expected:** 全部 PASS（允许既有 ignored tests，不允许新增 fail/error）。

---

## Execution Notes
- 严格 TDD：RED → GREEN → Regression。
- 不写脚本，不改 CI/DI。
- 每步回报命令输出。

---

## Execution Record (2026-02-11)

### Task 1 (P0): Add failing strict roundtrip assertions
- Modified: `tests/test_capability_deserialization_roundtrip.pas`
- RED command:
```bash
fpc -Fu./src tests/test_capability_deserialization_roundtrip.pas -otmp/test_cap_roundtrip && ./tmp/test_cap_roundtrip
```
- Output (key):
  - `❌ json.sessionCacheSupport mismatch: expected=2 actual=0`

### Task 2 (P0): Implement serializer/deserializer symmetry
- Modified: `src/fafafa.ssl.capability.serializer.pas`
- Changes:
  - Added JSON/XML roundtrip fields:
    - support-level: `SessionCacheSupport`, `ZeroRTTSupport`, `EarlyDataSupport`, `RenegotiationSupport`, `PostHandshakeAuthSupport`
    - sets: `SupportedCiphers`, `SupportedHashes`, `SupportedKeyExchanges`
    - key/cert: `SupportsDERPrivateKey`, `SupportsPKCS8PrivateKey`, `SupportsPKCS12`, `SupportsPasswordProtectedKeys`
    - extensibility: `SupportsCustomCipherSuites`, `SupportsCallbacks`
  - Added stable set codec (`Encode*/Decode*`) for JSON/XML symmetry.
- GREEN command:
```bash
fpc -Fu./src tests/test_capability_deserialization_roundtrip.pas -otmp/test_cap_roundtrip && ./tmp/test_cap_roundtrip
```
- Output (key):
  - `✅ JSON round-trip passed`
  - `✅ XML round-trip passed`

### Task 3: Regression + P0-18 gate
- Commands:
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
- Output (key):
  - `✅ FreePascal backend basic checks passed`
  - `✅ FreePascal server accept skeleton checks passed`
  - `Number of run tests: 10`, `Number of failures: 0`, `Number of errors: 0`

### Status
- `P0-17`: complete
- `P0-18`: complete
- P0 track: cleared (1-18 complete)
- Next candidate: `P1-19 WolfSSL certificate IsCA basic-constraints accuracy`
