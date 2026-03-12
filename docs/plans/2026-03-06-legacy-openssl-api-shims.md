# Legacy OpenSSL API Shim Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为历史 OpenSSL 导入名（如 `fafafa.ssl.openssl.types`、`fafafa.ssl.openssl.core`）补齐兼容 shim，让仍使用旧命名的活跃示例和测试能继续编译。

**Architecture:** 先用 shell 契约锁定“活跃 Pascal 源中引用的旧 OpenSSL 单元必须在 `src/` 中有对应 shim 文件”。然后自动生成一组轻量 wrapper 单元，把旧导入名转发到现有 `fafafa.ssl.openssl.api.*` 实现；对少数特殊项（如 `cmac`、`sha3`）映射到更合适的目标单元。最后用几个代表性旧示例/测试做编译验证。

**Tech Stack:** shell contract tests, FreePascal wrapper units, legacy-to-canonical unit mapping.

---

### Task 1: Lock legacy shim coverage with a failing contract

**Files:**
- Create: `tests/scripts/test_legacy_openssl_api_shim_coverage_contract.sh`

**Step 1: Write failing contract**
- Enumerate legacy OpenSSL imports from active Pascal source under `src/`, `tests/`, `examples/`.
- Assert each referenced legacy unit has a matching `src/<unit>.pas` shim file, excluding canonical `api.*`, `base`, `loader`, `lib`, `backed`, and already-real units.

**Step 2: Run test to verify RED**
- Run: `bash tests/scripts/test_legacy_openssl_api_shim_coverage_contract.sh`
- Expected: FAIL before shim files exist.

### Task 2: Add minimal wrapper units

**Files:**
- Create: compatibility shim units under `src/`

**Step 1: Generate wrappers**
- For direct mappings, create wrapper units whose interface `uses` the corresponding `fafafa.ssl.openssl.api.<name>` unit.
- For special cases, map:
  - `fafafa.ssl.openssl.cmac` → `fafafa.ssl.openssl.api.cmac.evp`
  - `fafafa.ssl.openssl.sha3` → `fafafa.ssl.openssl.api.sha3.evp`

**Step 2: Re-run contract**
- Run the same shell contract.
- Expected: PASS.

### Task 3: Verify representative legacy imports compile

**Files:**
- Verify only

**Step 1: Compile representative old-import programs**
- Run: `fpc -Fu./src examples/test_ssl_context.lpr -otmp/test_ssl_context`
- Run: `fpc -Fu./src examples/test_openssl_rsa.lpr -otmp/test_openssl_rsa`
- Run: `fpc -Fu./src examples/test_pem.lpr -otmp/test_pem`
- Expected: PASS.
