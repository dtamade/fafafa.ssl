# Stream Connection Legacy "not implemented" Cleanup Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 清理 `test_stream_connection` 中 legacy `"not implemented" -> SKIP` 分支，改为能力驱动分类，避免掩盖真实契约退化。

**Architecture:** 先添加分类契约红测（`not implemented` 不应被视作 capability skip）；再实现统一分类 helper 并替换各后端 catch 分支；最后回归。

**Tech Stack:** FreePascal (ObjFPC), `tests/test_stream_connection.pas`.

---

### Task 1 (P0): Add failing classification contract
- Files: `tests/test_stream_connection.pas`
- RED command:
  - `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- Expected: FAIL（helper 未实现或 legacy 分类不符合新契约）。

### Task 2 (P0): Implement capability-driven skip classifier
- Files: `tests/test_stream_connection.pas`
- GREEN command:
  - `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
- Expected: PASS（`not implemented` 不再映射为 SKIP）。

### Task 3 (P1): Focused regression
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept`
