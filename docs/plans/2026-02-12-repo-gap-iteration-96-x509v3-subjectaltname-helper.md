# X509V3 SubjectAltName Helper Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 实现 `X509AddSubjectAltName` 的真实行为，消除当前占位 `Result := False` 缺口。

**Architecture:** 先新增运行时合同（helper 成功 + 扩展可观测），再做最小实现（`subjectAltName` 扩展构造并附加），最后执行回归链验证无副作用。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL X509/X509V3 APIs, program-style tests.

---

## Scan Summary (2026-02-12)
- `src/fafafa.ssl.openssl.api.x509v3.pas:429` 的 `X509AddSubjectAltName` 仍为占位实现（固定 `Result := False`）。
- 相关 helper（`X509AddBasicConstraints` / `X509AddKeyUsage` / `X509AddExtKeyUsage`）已落地真实附加语义并有合同保护，SAN helper 成为同组最后高信号缺口。
- 优先级：**P0**（直接影响证书构造时 SAN 能力可用性）。

---

### Task 1: Add failing contract (RED)

**Files:**
- Add: `tests/test_x509v3_subjectaltname_contract.pas`

**Step 1: Write failing test**
- `X509_new()` 创建证书并设置 version=2。
- 调用 `X509AddSubjectAltName(Cert, 'localhost')`。
- 断言：
  - helper 返回 `True`
  - 扩展计数 +1
  - `NID_subject_alt_name` 可查询

**Step 2: Run RED**
- `fpc -Fu./src tests/test_x509v3_subjectaltname_contract.pas -otmp/test_x509v3_subjectaltname_contract`
- `./tmp/test_x509v3_subjectaltname_contract`
- Expected: FAIL（当前 helper 占位）。

---

### Task 2: Minimal implementation (GREEN)

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.x509v3.pas`

**Step 1: Implement helper**
- 入参校验：`Cert<>nil` 且 `Trim(DNS)<>''`。
- 若输入未带前缀，构造 `DNS:<name>`；若已是 `DNS:` 前缀则直接使用。
- 调用 `X509V3_EXT_conf_nid(..., NID_subject_alt_name, value)` 创建扩展。
- 用 `X509_add_ext` 附加扩展并在 finally 中 `X509_EXTENSION_free`。
- 仅附加成功返回 `True`。

**Step 2: Run GREEN**
- `fpc -Fu./src tests/test_x509v3_subjectaltname_contract.pas -otmp/test_x509v3_subjectaltname_contract && ./tmp/test_x509v3_subjectaltname_contract`
- Expected: PASS。

---

### Task 3: Regression

- `fpc -Fu./src tests/test_x509v3_extkeyusage_contract.pas -otmp/test_x509v3_extkeyusage_contract && ./tmp/test_x509v3_extkeyusage_contract`
- `fpc -Fu./src tests/test_x509v3_keyusage_contract.pas -otmp/test_x509v3_keyusage_contract && ./tmp/test_x509v3_keyusage_contract`
- `fpc -Fu./src tests/test_x509v3_basicconstraints_contract.pas -otmp/test_x509v3_basicconstraints_contract && ./tmp/test_x509v3_basicconstraints_contract`
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract`
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Execution Record (2026-02-12)

### RED
- Command:
  - `fpc -Fu./src tests/test_x509v3_subjectaltname_contract.pas -otmp/test_x509v3_subjectaltname_contract && ./tmp/test_x509v3_subjectaltname_contract`
- Result: FAIL
- Key output:
  - `X509AddSubjectAltName should report success: FAIL`
  - `Extension count should increase by one: FAIL` (`before=0, after=0`)
  - `SubjectAltName extension should be queryable by NID: FAIL` (`index=-1`)

### GREEN
- Modified files:
  - `src/fafafa.ssl.openssl.api.x509v3.pas`
  - `tests/test_x509v3_subjectaltname_contract.pas`
- Implementation:
  - Implemented `X509AddSubjectAltName` input validation (`Cert` + trimmed `DNS`).
  - Normalized value to `DNS:<name>` format.
  - Created SAN extension via `X509V3_EXT_conf_nid(..., NID_subject_alt_name, value)`.
  - Attached extension with `X509_add_ext` and released via `X509_EXTENSION_free`.
- GREEN command:
  - `fpc -Fu./src tests/test_x509v3_subjectaltname_contract.pas -otmp/test_x509v3_subjectaltname_contract && ./tmp/test_x509v3_subjectaltname_contract`
- GREEN result:
  - `Total tests: 3`
  - `Passed: 3`
  - `Failed: 0`
  - `Skipped: 0`

### Regression
- `fpc -Fu./src tests/test_x509v3_extkeyusage_contract.pas -otmp/test_x509v3_extkeyusage_contract && ./tmp/test_x509v3_extkeyusage_contract` -> PASS（5/5）
- `fpc -Fu./src tests/test_x509v3_keyusage_contract.pas -otmp/test_x509v3_keyusage_contract && ./tmp/test_x509v3_keyusage_contract` -> PASS（5/5）
- `fpc -Fu./src tests/test_x509v3_basicconstraints_contract.pas -otmp/test_x509v3_basicconstraints_contract && ./tmp/test_x509v3_basicconstraints_contract` -> PASS（3/3）
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract` -> PASS（1/1）
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise` -> PASS（48/48）
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn` -> PASS（10/0/1）
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all` -> PASS（N:10 E:0 F:0 I:2）
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS（15/15）
  - Report: `test-reports/test_report_20260212_170709.txt`
