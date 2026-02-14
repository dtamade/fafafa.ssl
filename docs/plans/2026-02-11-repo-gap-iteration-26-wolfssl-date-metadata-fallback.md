# WolfSSL Date Metadata Decoding Robustness Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 当 wolfSSL ASN1 时间 API 不可用时，`GetNotBefore/GetNotAfter` 仍能通过 DER fallback 提供可用日期，避免测试长期 SKIP。

**Architecture:** 先把 metadata 测试中的日期检查改为强断言制造 RED；再在 `TWolfSSLCertificate.GetNotBefore/GetNotAfter` 增加 `TX509Certificate` fallback；最后回归。

**Tech Stack:** FreePascal (ObjFPC), `src/fafafa.ssl.wolfssl.certificate.pas`, `tests/connection/test_wolfssl_metadata_accuracy.pas`, `fafafa.ssl.x509`.

---

### Task 1 (P1): Tighten date assertions to fail on missing decode
- Modify: `tests/connection/test_wolfssl_metadata_accuracy.pas`
- RED command:
  - `fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy`
- Expected: FAIL（当前环境 ASN1 API 路径返回 0）。

### Task 2 (P1): Add DER fallback in GetNotBefore/GetNotAfter
- Modify: `src/fafafa.ssl.wolfssl.certificate.pas`
- GREEN command:
  - same as RED
- Expected: PASS。

### Task 3 (P1): Focused regression
- `fpc -Fu./src tests/test_wolfssl_framework.pas -otmp/test_wolfssl_framework && ./tmp/test_wolfssl_framework`
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`

---

## Execution Record (2026-02-11 11:10 +0800)

### Task 1 (P1): Tighten date assertions
- Modified: `tests/connection/test_wolfssl_metadata_accuracy.pas`
- RED command:
```bash
fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy
```
- Output (key):
  - `❌ NotBefore should be decoded (wolfSSL API or DER fallback)`

### Task 2 (P1): Add fallback in GetNotBefore/GetNotAfter
- Modified: `src/fafafa.ssl.wolfssl.certificate.pas`
- Initial attempt issues:
  1) compile error (`TX509Certificate` has `Validity.NotBefore/NotAfter`, no direct `NotBefore/NotAfter` members)
  2) runtime AV on mixed wolfSSL ASN1 + DER fallback path
- Final minimal implementation:
  - `GetNotBefore/GetNotAfter` use safe PEM-based fallback parsing (`TX509Certificate.LoadFromPEM`) to decode validity.
  - Avoid unstable branch that triggered AV in this runtime.
- GREEN command:
```bash
fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy
```
- Output (key):
  - `✅ wolfssl metadata accuracy tests passed`

### Task 3 (P1): Regression
```bash
fpc -Fu./src tests/test_wolfssl_framework.pas -otmp/test_wolfssl_framework && ./tmp/test_wolfssl_framework
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
- Output (key):
  - `WolfSSL Framework Test Summary ... Failed: 0`
  - `✅ FreePascal backend basic checks passed`
  - `Number of run tests: 10 / failures: 0 / errors: 0`

### Status
- `P1-21`: complete
- Next candidate: `P1-22 WolfSSL session serialize/deserialize nil-safety`
