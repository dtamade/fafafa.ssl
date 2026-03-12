# CertificateUtils VerifyChain Intermediate Chain Loading Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 修复 `TCertificateUtils.VerifyChain` 对“leaf + intermediate”链输入的处理中间证书导入缺口，保证链验证可通过。

**Architecture:** 先新增独立合同测试重现 `leaf+intermediate` 验证失败，再最小修复 `VerifyChain` 的 PEM 读取指针逻辑，最后跑证书与核心回归。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL BIO/PEM API, program-style tests.

---

## Scan Summary (2026-02-12)

### High-signal gap
1. `VerifyChain` 在提取中间证书时调用 `BIO_get_mem_data` 的指针使用不正确（将返回指针写入字符数组地址后又当作内容读取）。
2. 该缺陷导致 bundled chain (`leaf + intermediate`) 场景中中间证书导入失败。
3. 实测复现：`leaf-only=false`（预期），`leaf+inter=false`（异常，预期应 true）。

### Priority
- **P0:** `VerifyChain` intermediate chain contract fix（test-first）。

---

### Task 1: Add failing chain contract (RED)

**Files:**
- Add: `tests/test_cert_utils_verify_chain_contract.pas`

**Step 1: Write failing test**
- 动态生成 Root CA / Intermediate CA / Leaf 证书。
- 断言：
  - `VerifyChain(leaf, root)` 为 `False`（缺中间证书）。
  - `VerifyChain(leaf + intermediate, root)` 为 `True`（链完整）。

**Step 2: Run RED**
- `fpc -Fu./src tests/test_cert_utils_verify_chain_contract.pas -otmp/test_cert_utils_verify_chain_contract`
- `./tmp/test_cert_utils_verify_chain_contract`
- Expected: 第二条断言 FAIL。

---

### Task 2: Minimal implementation (GREEN)

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Step 1: Fix intermediate PEM extraction**
- `VerifyChain` 中使用 `PAnsiChar` 指针接收 `BIO_get_mem_data` 返回值。
- `SetString` 直接从该数据指针构造 PEM 字符串。
- 保持其余验证流程不变。

**Step 2: Run GREEN**
- `fpc -Fu./src tests/test_cert_utils_verify_chain_contract.pas -otmp/test_cert_utils_verify_chain_contract`
- `./tmp/test_cert_utils_verify_chain_contract`
- Expected: PASS。

---

### Task 3: Focused regression

**Step 1: Cert utils Ed25519 contract**
- `fpc -Fu./src tests/test_cert_utils_ed25519_contract.pas -otmp/test_cert_utils_ed25519_contract`
- `./tmp/test_cert_utils_ed25519_contract`

**Step 2: Core regression**
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn`
- `./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple`
- `./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`


## Execution Log (2026-02-12)

### Task 1 RED
- Command:
  - `fpc -Fu./src tests/test_cert_utils_verify_chain_contract.pas -otmp/test_cert_utils_verify_chain_contract && ./tmp/test_cert_utils_verify_chain_contract`
- Result: **FAIL** (expected)
- Key failure:
  - `VerifyChain(leaf+intermediate, root) should succeed -> FAIL`

### Task 2 GREEN
- Files modified:
  - `src/fafafa.ssl.cert.utils.pas`
  - `src/fafafa.ssl.certchain.pas`
- Implementation:
  - `VerifyChain` intermediate PEM extraction fixed to use correct BIO data pointer.
  - `BuildChain` switched from raw-pointer `TList` to ref-count-safe `TInterfaceList`.
  - `CheckCertificateSignature` adjusted to prioritize intermediate-store validation and trusted-store fallback, with issuer relation gate.
- Command:
  - `fpc -Fu./src tests/test_cert_utils_verify_chain_contract.pas -otmp/test_cert_utils_verify_chain_contract && ./tmp/test_cert_utils_verify_chain_contract`
- Result: **PASS** (`Total tests: 5, Passed: 5, Failed: 0`)

### Task 3 Regression
- `fpc -Fu./src tests/test_cert_utils_ed25519_contract.pas -otmp/test_cert_utils_ed25519_contract && ./tmp/test_cert_utils_ed25519_contract` -> PASS（24/24）
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn` -> PASS（10/0/1）
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all` -> PASS（N:10 E:0 F:0 I:2）
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS（15/15）
  - Report: `docs/archive/reports/test-report-history/test_report_20260212_154040.txt`
