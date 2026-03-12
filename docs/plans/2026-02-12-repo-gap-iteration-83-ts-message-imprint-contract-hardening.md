# TS Message-Imprint Contract Hardening Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 修复 `CreateTimestampRequest` 中 message-imprint 的算法与摘要内容缺口，确保 RFC3161 请求中的 digest 算法与 digest 内容正确落地。

**Architecture:** 先在 `test_tsa_api` 加红测（算法 OID 必须为 SHA256、digest 长度/内容必须匹配 `SHA256(Data)`），再最小修改 `api.ts` 与 `api.x509` 绑定，最后跑 TS 专项 + 核心回归链。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL TS API, OpenSSL X509/EVP bindings.

---

## Scan Summary (2026-02-12)
- 发现缺口：`CreateTimestampRequest` 仅调用 `TS_MSG_IMPRINT_set_msg`，未正确设置算法，且对 `set_msg/set_msg_imprint` 返回值未做失败收敛。
- 红测现象：
  - 算法 `OBJ_obj2nid` 非 `NID_sha256`
  - imprint digest 长度不是 32（SHA256）

---

### Task 1 (P0): Add failing imprint contracts

**Files:**
- Modify: `tests/certificate/test_tsa_api.pas`

**RED assertions:**
1. `Request msg imprint algorithm is SHA256`
2. `Request msg imprint digest length matches SHA256`
3. `Request msg imprint digest bytes match SHA256(Data)`

**RED command:**
- `fpc -Fu./src tests/certificate/test_tsa_api.pas -otmp/test_tsa_api && ./tmp/test_tsa_api`
- key failures:
  - `Request msg imprint algorithm is SHA256... FAIL`
  - `Request msg imprint digest length matches SHA256... FAIL`

---

### Task 2 (P0): Minimal implementation

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.ts.pas`
- Modify: `src/fafafa.ssl.openssl.api.x509.pas`

**Change set:**
- `api.x509` 增加 `X509_ALGOR_set_md` 动态绑定。
- `CreateTimestampRequest` 中 message-imprint 路径：
  - digest 计算改为显式 fail-safe 路径，`Data` 空时用 `nil` 指针 + `len=0`
  - 使用 `X509_ALGOR_set_md` 设置 imprint 算法为 `EVP_sha256`
  - 严格检查 `TS_MSG_IMPRINT_set_msg` 与 `TS_REQ_set_msg_imprint` 返回值；失败即 `Exit(nil)`

**GREEN command:**
- same as RED command
- expected: `20/20` 全绿。

---

### Task 3 (P1): Focused regression
- `fpc -Fu./src tests/certificate/test_p2_ts_comprehensive.pas -otmp/test_p2_ts_comprehensive && ./tmp/test_p2_ts_comprehensive`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Execution Record (2026-02-12 13:42 +0800)
- RED reproduced ✅
- GREEN passed ✅
- TS comprehensive + core regressions passed ✅
  - module report: `docs/archive/reports/test-report-history/test_report_20260212_134110.txt`
