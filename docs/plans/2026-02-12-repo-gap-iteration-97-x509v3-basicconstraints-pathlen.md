# X509V3 BasicConstraints PathLen Semantics Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 补齐 `X509AddBasicConstraints` 的 `PathLen` 语义，避免当前 `PathLen>=0` 仍被静默忽略。

**Architecture:** 先新增运行时合同验证 `PathLen=0` 可被 `X509_get_pathlen` 观测；再做最小实现（ASN1 INTEGER 注入 pathlen 字段）；最后执行 x509v3+核心回归链。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL X509/X509V3/ASN1 APIs, program-style tests.

---

## Scan Summary (2026-02-12)
- `src/fafafa.ssl.openssl.api.x509v3.pas:315-318` 存在注释：`PathLen assignment requires ASN1_INTEGER helpers`，当前未实现。
- 该缺口会导致 `X509AddBasicConstraints(CA=true, PathLen>=0)` 的路径长度约束失效（退化为 unlimited）。
- 优先级：**P0**（证书约束语义正确性）。

---

### Task 1: Add failing contract (RED)

**Files:**
- Add: `tests/test_x509v3_basicconstraints_pathlen_contract.pas`

**Step 1: Write failing test**
- 创建证书并设置 version=2。
- 调用 `X509AddBasicConstraints(Cert, True, 0)`。
- 断言：
  - helper 返回 `True`
  - `X509_get_pathlen(Cert) = 0`

**Step 2: Run RED**
- `fpc -Fu./src tests/test_x509v3_basicconstraints_pathlen_contract.pas -otmp/test_x509v3_basicconstraints_pathlen_contract`
- `./tmp/test_x509v3_basicconstraints_pathlen_contract`
- Expected: FAIL（当前 pathLen 被忽略）。

---

### Task 2: Minimal implementation (GREEN)

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.x509v3.pas`

**Step 1: Implement pathlen assignment**
- 在 `PathLen >= 0` 分支加载 ASN1 helpers（`LoadOpenSSLASN1`）。
- 用 `ASN1_INTEGER_new + ASN1_INTEGER_set` 构造 pathlen 并挂到 `BASIC_CONSTRAINTS.pathlen`。
- 保持现有错误回退语义，失败时返回 `False`。

**Step 2: Run GREEN**
- `fpc -Fu./src tests/test_x509v3_basicconstraints_pathlen_contract.pas -otmp/test_x509v3_basicconstraints_pathlen_contract && ./tmp/test_x509v3_basicconstraints_pathlen_contract`
- Expected: PASS。

---

### Task 3: Regression

- `fpc -Fu./src tests/test_x509v3_subjectaltname_contract.pas -otmp/test_x509v3_subjectaltname_contract && ./tmp/test_x509v3_subjectaltname_contract`
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
- Initial compile RED:
  - `Identifier not found "clong"`（测试类型声明问题，修正为 `Int64` 后继续 RED）
- Semantic RED command:
  - `fpc -Fu./src tests/test_x509v3_basicconstraints_pathlen_contract.pas -otmp/test_x509v3_basicconstraints_pathlen_contract && ./tmp/test_x509v3_basicconstraints_pathlen_contract`
- Semantic RED result:
  - `X509AddBasicConstraints should report success: PASS`
  - `PathLen should be 0 when helper is called with PathLen=0: FAIL` (`pathLen=-1`)

### GREEN
- Modified files:
  - `src/fafafa.ssl.openssl.api.x509v3.pas`
  - `tests/test_x509v3_basicconstraints_pathlen_contract.pas`
- Implementation:
  - Added ASN1 dependency in `x509v3` implementation uses.
  - In `X509AddBasicConstraints` pathLen branch:
    - load ASN1 symbols via `LoadOpenSSLASN1(GetCryptoLibHandle)`
    - allocate/set integer via `ASN1_INTEGER_new + ASN1_INTEGER_set`
    - assign `BASIC_CONSTRAINTS.pathlen` with generated ASN1 integer.
- GREEN command:
  - `fpc -Fu./src tests/test_x509v3_basicconstraints_pathlen_contract.pas -otmp/test_x509v3_basicconstraints_pathlen_contract && ./tmp/test_x509v3_basicconstraints_pathlen_contract`
- GREEN result:
  - `Total tests: 2`
  - `Passed: 2`
  - `Failed: 0`
  - `Skipped: 0`

### Regression
- `fpc -Fu./src tests/test_x509v3_subjectaltname_contract.pas -otmp/test_x509v3_subjectaltname_contract && ./tmp/test_x509v3_subjectaltname_contract` -> PASS（3/3）
- `fpc -Fu./src tests/test_x509v3_extkeyusage_contract.pas -otmp/test_x509v3_extkeyusage_contract && ./tmp/test_x509v3_extkeyusage_contract` -> PASS（5/5）
- `fpc -Fu./src tests/test_x509v3_keyusage_contract.pas -otmp/test_x509v3_keyusage_contract && ./tmp/test_x509v3_keyusage_contract` -> PASS（5/5）
- `fpc -Fu./src tests/test_x509v3_basicconstraints_contract.pas -otmp/test_x509v3_basicconstraints_contract && ./tmp/test_x509v3_basicconstraints_contract` -> PASS（3/3）
- `fpc -Fu./src tests/test_x509v3_basicconstraints_pathlen_contract.pas -otmp/test_x509v3_basicconstraints_pathlen_contract && ./tmp/test_x509v3_basicconstraints_pathlen_contract` -> PASS（2/2）
- `fpc -Fu./src tests/test_x509_ext_count_contract.pas -otmp/test_x509_ext_count_contract && ./tmp/test_x509_ext_count_contract` -> PASS（1/1）
- `fpc -Fu./src tests/certificate/test_x509_enterprise.pas -otmp/test_x509_enterprise && ./tmp/test_x509_enterprise` -> PASS（48/48）
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn` -> PASS（10/0/1）
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all` -> PASS（N:10 E:0 F:0 I:2）
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS（15/15）
  - Report: `test-reports/test_report_20260212_171744.txt`
