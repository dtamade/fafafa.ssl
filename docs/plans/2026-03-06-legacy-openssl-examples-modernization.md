# Legacy OpenSSL Examples Modernization Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让两个代表性的旧 OpenSSL 示例 `examples/test_openssl_rsa.lpr` 与 `examples/test_pem.lpr` 在当前 API 命名下重新恢复可编译状态。

**Architecture:** 先用 shell 契约锁定这两个示例必须能编译。然后只做示例级最小修复：`test_openssl_rsa.lpr` 修正静态数组上错误使用 `SetLength` 的示例 bug；`test_pem.lpr` 切到当前可用的 loader 与类型来源（`api`、`LoadOpenSSLRSA`、`LoadEVP(GetCryptoLibHandle)`、`LoadOpenSSLBIO`）。

**Tech Stack:** shell contract tests, FreePascal examples, OpenSSL api units.

---

### Task 1: Lock example compile behavior

**Files:**
- Create: `tests/scripts/test_legacy_openssl_examples_compile_contract.sh`

**Step 1: Write failing contract**
- Compile `examples/test_openssl_rsa.lpr`
- Compile `examples/test_pem.lpr`
- Expect RED before example modernization.

### Task 2: Modernize example-local code only

**Files:**
- Modify: `examples/test_openssl_rsa.lpr`
- Modify: `examples/test_pem.lpr`

**Step 1: RSA example**
- Replace the invalid static-array `SetLength` usage with a dynamic `TBytes` buffer.

**Step 2: PEM example**
- Import `fafafa.ssl.openssl.api` for legacy pointer aliases.
- Replace removed helper loaders with current APIs:
  - `LoadOpenSSLRSA`
  - `LoadEVP(GetCryptoLibHandle)`
  - `LoadOpenSSLBIO`

### Task 3: Verify

**Files:**
- Verify only

**Step 1: Re-run the shell contract**
- `bash tests/scripts/test_legacy_openssl_examples_compile_contract.sh`
- Expected: PASS.
