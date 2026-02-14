# Backend Contract FreePascal Parity Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 修复 `test_backend_contract` 中 FreePascal backend 的“误跳过 + SAN/Hostname 语义漂移”，让该后端在合同测试中与 OpenSSL/WolfSSL/MbedTLS 行为对齐。

**Architecture:** 先加红测（harness 校验 direct-instance helper 覆盖 FreePascal）；再最小实现 helper 分支；随后修复 FreePascal 证书 SAN 提取（包含 IP SAN），消除 `Cert_SAN_Parsing` 与 `Cert_VerifyHostname` 失败；最后执行定向回归。

**Tech Stack:** FreePascal (ObjFPC), `tests/contract/test_backend_contract.pas`, `src/fafafa.ssl.freepascal.lib.pas`.

---

## Scan Summary (2026-02-12)
- `tests/contract/test_backend_contract.pas` 的 `TryCreateDirectLibraryInstance` 未覆盖 `sslFreePascal`，导致 FreePascal contract-1 被误标为“平台不支持”。
- 在补齐 direct helper 后，合同测试暴露真实语义缺口：
  - `Cert_SAN_Parsing [FreePascal Native]: Missing expected SAN entries`
  - `Cert_VerifyHostname [FreePascal Native]: VerifyHostname mismatch`
- 根因：`TFreePascalCertificate.RebuildInfo` 只用 `GetDNSNames` 回填 SAN，遗漏 IP SAN。

---

### Task 1: Add failing harness contract (RED)

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`

**Step 1: Write failing harness assertion**
- 新增 `TestHarness_DirectInstanceCoverage_FreePascal`：
  - 断言 `TryCreateDirectLibraryInstance(sslFreePascal, ...)` 必须成功。

**Step 2: Run RED**
- `fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract`
- `./tmp/test_backend_contract`
- Expected: FAIL（helper 未处理 `sslFreePascal`）。

---

### Task 2: Minimal helper implementation (GREEN-1)

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`

**Step 1: Implement helper support**
- `uses` 中引入 `fafafa.ssl.freepascal.lib`。
- 在 `TryCreateDirectLibraryInstance` 增加：
  - `sslFreePascal: ALib := TFreePascalSSLLibrary.Create;`

**Step 2: Run test**
- 重新运行 `test_backend_contract`。
- 预期 harness 通过，但可能暴露下一层语义缺口。

---

### Task 3: Close SAN/VerifyHostname parity gap (GREEN-2)

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Implement SAN extraction parity**
- `RebuildInfo` 从 `LParser.SubjectAltNames` 回填全部 SAN 值（而非仅 `GetDNSNames`），覆盖 DNS + IP。

**Step 2: Run test to verify GREEN**
- `fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract`
- Expected: PASS（FreePascal `Cert_SAN_Parsing` + `Cert_VerifyHostname` 通过）。

---

### Task 4: Regression

- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_freepascal_backend_basic && ./tmp/test_freepascal_backend_basic`
- `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
- `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

---

## Execution Record (2026-02-12)

### RED
- `fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract`
- Key output:
  - `Harness_DirectInstanceCoverage_FreePascal ... FAIL`
  - `Direct instance helper does not handle sslFreePascal`

### GREEN-1
- `tests/contract/test_backend_contract.pas`
  - Added FreePascal backend unit import.
  - Added `sslFreePascal` branch in `TryCreateDirectLibraryInstance`.
- Re-run output exposed deeper parity gap:
  - `Cert_SAN_Parsing [FreePascal Native]: Missing expected SAN entries`
  - `Cert_VerifyHostname [FreePascal Native]: VerifyHostname mismatch`

### GREEN-2
- `src/fafafa.ssl.freepascal.lib.pas`
  - `RebuildInfo` switched from `GetDNSNames` to `SubjectAltNames` full extraction.
- GREEN command:
  - `fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract`
- GREEN result:
  - `Total Tests: 47`
  - `Passed: 41`
  - `Failed: 0`
  - `Skipped: 6`

### Regression
- `test_freepascal_backend_basic` -> PASS
- `test_freepascal_server_accept_skeleton` -> PASS
- `test_stream_connection` -> PASS（10/0/1）
- `run_unit_tests_simple --all` -> PASS（N:10 E:0 F:0 I:2）
- `run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` -> PASS（15/15）
  - Report: `test-reports/test_report_20260212_181218.txt`
