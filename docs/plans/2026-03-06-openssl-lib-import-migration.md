# OpenSSL Lib Import Migration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 将活跃 Pascal 源中的 `fafafa.ssl.openssl.backed` 导入批量迁移到 `fafafa.ssl.openssl.lib`，同时保留旧单元作为兼容实现。

**Architecture:** 先用 shell 契约测试锁定“活跃 Pascal 源不应再直接导入 `fafafa.ssl.openssl.backed`”。然后只在 `src/`、`tests/`、`examples/` 的 `.pas/.lpr` 文件里做字符串级迁移，不改历史文档或兼容实现文件。最后通过契约扫描和代表性编译确认迁移没有破坏当前使用方式。

**Tech Stack:** shell contract tests, FreePascal unit imports, compatibility shim.

---

### Task 1: Lock canonical import usage with a failing contract

**Files:**
- Create: `tests/scripts/test_openssl_lib_canonical_imports_contract.sh`

**Step 1: Write failing contract**
- Assert active Pascal source files under `src/`, `tests/`, and `examples/` no longer reference `fafafa.ssl.openssl.backed`.
- Exclude the legacy implementation file and the shim unit itself.

**Step 2: Run test to verify RED**
- Run: `bash tests/scripts/test_openssl_lib_canonical_imports_contract.sh`
- Expected: FAIL before migration.

### Task 2: Batch-migrate Pascal imports

**Files:**
- Modify: active `.pas/.lpr` files under `src/`, `tests/`, `examples/`

**Step 1: Replace direct imports**
- Swap `fafafa.ssl.openssl.backed` → `fafafa.ssl.openssl.lib` in active Pascal source only.

**Step 2: Re-run contract**
- Run: `bash tests/scripts/test_openssl_lib_canonical_imports_contract.sh`
- Expected: PASS.

### Task 3: Verify representative compilation

**Files:**
- Verify only

**Step 1: Representative compiles**
- Run: `fpc -Fu./src tests/test_api_check.pas -otmp/test_api_check`
- Run: `fpc -Fu./src tests/test_convenience_methods.pas -otmp/test_convenience_methods`
- Run: `fpc -Fu./src tests/test_backend_selector_basic.pas -otmp/test_backend_selector_basic`
- Expected: PASS.
