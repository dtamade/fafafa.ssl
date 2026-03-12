# Cert Utils Ed25519 Support Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在 `TCertificateUtils` 中补齐 `ktEd25519` 证书生成功能，使自签名与 CA 签发流程可用，不再固定走 unsupported。

**Architecture:** 先用合同测试将 Ed25519 自签名/签发路径拉红；再最小实现 `GenerateEd25519Key` 与签名摘要选择逻辑（Ed25519 用 raw sign）；最后执行证书与核心回归。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL EVP/X509 APIs, `fafafa.ssl.cert.utils`.

---

## Scan Summary (2026-02-12)
- 高信号缺口：`src/fafafa.ssl.cert.utils.pas` 在 `GenerateSelfSigned/GenerateSigned` 对 `ktEd25519` 直接 `RaiseUnsupported`。
- 当前仓库已具备条件：`src/fafafa.ssl.openssl.api.evp.pas` 已暴露 `EVP_PKEY_ED25519`、`EVP_PKEY_CTX_new_id`、`EVP_PKEY_keygen_init`、`EVP_PKEY_keygen`。

---

### Task 1 (P0): Add failing Ed25519 contracts

**Files:**
- Modify: `tests/test_cert_utils_ed25519_contract.pas`

**Step 1: Write failing tests**
- `TestEd25519SelfSignedContract`
  - `GenerateSelfSigned` with `ktEd25519` should succeed
  - 输出包含 PEM 证书/私钥头
  - `GetInfo` 的 `PublicKeyType` 应包含 `25519`
- `TestEd25519CASignedLeafContract`
  - RSA CA + Ed25519 leaf 的 `GenerateSigned` 应成功

**Step 2: Verify RED**
- `fpc -Fu./src tests/test_cert_utils_ed25519_contract.pas -otmp/test_cert_utils_ed25519_contract && ./tmp/test_cert_utils_ed25519_contract`
- 期望失败：当前仍抛 `unsupported`。

---

### Task 2 (P0): Minimal implementation

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Step 1: Implement key generation + signing compatibility**
- 新增 `GenerateEd25519Key`（基于 `EVP_PKEY_CTX_new_id(EVP_PKEY_ED25519)` + keygen）
- `GenerateSelfSigned/GenerateSigned` 的 `ktEd25519` 分支改为调用该生成函数
- 新增最小签名适配：Ed25519/未知 raw-sign 情况下，`X509_sign` 使用 `nil` digest 回退

**Step 2: Verify GREEN**
- 重跑 Task 1 命令，期望 PASS。

---

### Task 3 (P1): Focused regression
- `fpc -Fu./src tests/test_cert_utils_ed25519_contract.pas -otmp/test_cert_utils_ed25519_contract && ./tmp/test_cert_utils_ed25519_contract`
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Acceptance
- `ktEd25519` 在 `TCertificateUtils` 的自签名/签发路径可执行。
- Ed25519 合同测试通过，且输出语义稳定。
- 关键回归链路全绿。

## Execution Record (2026-02-12 14:35 CST)

### RED
- `fpc -Fu./src tests/test_cert_utils_ed25519_contract.pas -otmp/test_cert_utils_ed25519_contract && ./tmp/test_cert_utils_ed25519_contract`
- 关键失败：
  - `FATAL: ESSLConfigurationException: Ed25519 key type is not supported by the current OpenSSL build or version.`

### GREEN
- 修改：
  - `src/fafafa.ssl.cert.utils.pas`
  - `tests/test_cert_utils_ed25519_contract.pas`
- 关键实现：
  - `GenerateEd25519Key`（EVP keygen）
  - `SignCertificateWithKey`（Ed25519 raw sign / SHA256 fallback）
  - `GenerateSelfSigned/GenerateSigned` 接入 `ktEd25519`
  - `GetInfo` 补齐 `PublicKeyType/PublicKeyBits` 提取
- GREEN 验证：
  - `Total tests: 10, Passed: 10, Failed: 0, Skipped: 0`

### Regression
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise` -> PASS（`48/48`）
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn` -> PASS（`10/0/1`）
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all` -> PASS（`N:10 E:0 F:0 I:2`）
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS（`15/15`, report `docs/archive/reports/test-report-history/test_report_20260212_143429.txt`）
