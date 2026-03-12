# OpenSSL Lib Compatibility Shim Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为 OpenSSL 后端引入更一致的规范单元名 `fafafa.ssl.openssl.lib`，同时保留 `fafafa.ssl.openssl.backed` 兼容性，避免大规模破坏式重命名。

**Architecture:** 先用一个最小 Pascal 编译测试锁定“新单元名必须可编译并暴露关键 OpenSSL library 管理符号”的契约。然后新增一个 shim 单元 `src/fafafa.ssl.openssl.lib.pas`，把 `TOpenSSLLibrary`、`CreateOpenSSLLibrary`、注册函数和路径配置函数代理到现有 `fafafa.ssl.openssl.backed`。最后把少量高可见度入口和文档切换到新规范名，旧单元继续保留作为兼容层。

**Tech Stack:** FreePascal (ObjFPC), Pascal unit aliasing via wrapper unit, markdown docs.

---

### Task 1: Lock canonical OpenSSL lib unit contract

**Files:**
- Create: `tests/test_openssl_lib_unit_compatibility.pas`
- Create: `src/fafafa.ssl.openssl.lib.pas`

**Step 1: Write failing test**
- Add a small Pascal program that imports `fafafa.ssl.openssl.lib`.
- Assert it can call `CreateOpenSSLLibrary`, instantiate `TOpenSSLLibrary`, and call the custom library path helper functions.

**Step 2: Run test to verify RED**
- Run: `fpc -Fu./src tests/test_openssl_lib_unit_compatibility.pas -otmp/test_openssl_lib_unit_compatibility && ./tmp/test_openssl_lib_unit_compatibility`
- Expected: FAIL because `fafafa.ssl.openssl.lib` does not exist yet.

**Step 3: Add shim unit**
- Create `src/fafafa.ssl.openssl.lib.pas`.
- Alias `TOpenSSLLibraryPaths` and `TOpenSSLLibrary`.
- Forward `CreateOpenSSLLibrary`, `RegisterOpenSSLBackend`, `UnregisterOpenSSLBackend`, and custom-path helper functions to `fafafa.ssl.openssl.backed`.

**Step 4: Re-run test to verify GREEN**
- Run the same command as Step 2.
- Expected: PASS.

### Task 2: Promote the canonical unit name in visible entry points

**Files:**
- Modify: `src/fafafa.ssl.pas`
- Modify: `docs/CAPABILITY_MATRIX_GUIDE.md`
- Modify: `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`

**Step 1: Update high-visibility references**
- Switch the main façade unit to depend on `fafafa.ssl.openssl.lib` instead of the legacy `backed` name.
- Update docs to describe `fafafa.ssl.openssl.lib` as the canonical import unit while noting that implementation still delegates to the legacy file.

### Task 3: Verify batch

**Files:**
- Verify only

**Step 1: Focused compile test**
- Run: `fpc -Fu./src tests/test_openssl_lib_unit_compatibility.pas -otmp/test_openssl_lib_unit_compatibility && ./tmp/test_openssl_lib_unit_compatibility`
- Expected: PASS.

**Step 2: Core compile gate**
- Run: `python3 scripts/compile_all_modules.py`
- Expected: PASS.
