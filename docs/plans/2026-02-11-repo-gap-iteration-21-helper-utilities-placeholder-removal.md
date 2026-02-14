# Helper Utilities Placeholder Assertion Removal Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 清理 `test_helper_utilities` 中 `GetCertificateInfo placeholder` 旧式占位断言，替换为可诊断的明确 setup failure。

**Architecture:** 先将 placeholder 分支切到未实现 helper 触发 RED；再实现 helper 并用单一、明确的失败记录替代占位断言；最后回归。

**Tech Stack:** FreePascal (ObjFPC), `tests/test_helper_utilities.pas`.

---

### Task 1 (P0): Add failing test path by replacing placeholder calls
- Files: `tests/test_helper_utilities.pas`
- RED command:
  - `fpc -Fu./src tests/test_helper_utilities.pas -otmp/test_helper_utils && ./tmp/test_helper_utils`
- Expected: FAIL（`ReportGetInfoSetupFailure` 未定义）。

### Task 2 (P0): Implement explicit setup-failure helper
- Files: `tests/test_helper_utilities.pas`
- GREEN command:
  - `fpc -Fu./src tests/test_helper_utilities.pas -otmp/test_helper_utils && ./tmp/test_helper_utils`
- Expected: PASS。

### Task 3 (P1): Focused regression
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
