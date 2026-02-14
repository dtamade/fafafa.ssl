# Native Handle Pure-Backend Contract Parity Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 `native_handle` 在 pure backend（FreePascal）路径下的 backend type 语义，让 `GetBackendType` 对库对象返回真实后端类型而非 `sslAutoDetect`。

**Architecture:** 在 `test_native_handle_unified` 增加 pure-backend 契约红测；最小修改 `GetBackendType` 增加 `ISSLLibrary` 回退分支；最后回归。

**Tech Stack:** FreePascal (ObjFPC), `src/fafafa.ssl.native_handle.pas`, `tests/test_native_handle_unified.pas`.

---

### Task 1 (P0): Add failing pure-backend contract test
- Files: `tests/test_native_handle_unified.pas`
- RED command:
  - `fpc -Fu./src tests/test_native_handle_unified.pas -otmp/test_native_handle_unified && ./tmp/test_native_handle_unified`
- Expected: FAIL（`GetBackendType` currently returns `sslAutoDetect` for pure backend library object）。

### Task 2 (P0): Implement library fallback in GetBackendType
- Files: `src/fafafa.ssl.native_handle.pas`
- GREEN command:
  - `fpc -Fu./src tests/test_native_handle_unified.pas -otmp/test_native_handle_unified && ./tmp/test_native_handle_unified`
- Expected: PASS。

### Task 3 (P1): Focused regression
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
