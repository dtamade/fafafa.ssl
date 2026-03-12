# 2026-03-10 builder material-loading helper extraction

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 抽出 `TSSLContextBuilderImpl.BuildClient` / `BuildServer` 共用的 material-loading helper，减少重复分支并保持当前已合同化行为不变。

**Architecture:** 这波是受控重构，不改用户面 contract。先用结构合同锁定“必须有共享 helper 且 client/server 都调用它”，再在 builder 中抽出统一 helper，覆盖 common config、certificate/private-key precedence、system roots、CA、cipher、ServerName、ALPN、session config；server-only identity requirement 通过参数控制。

**Tech Stack:** Free Pascal (ObjFPC), shell contract, focused Pascal regression suites.

---

### Task 1: Add failing structure contract

**Files:**
- Create: `tests/scripts/test_context_builder_material_loading_helper_contract.sh`

**Step 1: Write contract**
- Require builder source to contain:
  - helper declaration / implementation
  - `BuildClient` calling helper with client mode
  - `BuildServer` calling helper with server mode

**Step 2: Run RED**
- Run: `bash tests/scripts/test_context_builder_material_loading_helper_contract.sh`
- Expected: FAIL because helper does not exist yet.

### Task 2: Extract helper

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Introduce shared helper**
- Add a private helper that:
  - applies common config
  - optionally enforces server identity preconditions
  - loads certificate/private key using current precedence
  - applies trust material, cipher, SNI, ALPN, session config

**Step 2: Make `BuildClient` / `BuildServer` delegate**
- Keep context creation separate
- Replace duplicated inline branches with helper call

### Task 3: Verify no behavior drift

**Files:**
- Test: `tests/scripts/test_context_builder_material_loading_helper_contract.sh`
- Test: `tests/test_context_builder_backend_store_consistency.pas`
- Test: `tests/config/test_config_validation.pas`

**Step 1: Run focused suites**
- `bash tests/scripts/test_context_builder_material_loading_helper_contract.sh`
- `fpc -gl -Fu./src -Fu./tests -Fu./tests/helpers -otmp/test_context_builder_backend_store_consistency tests/test_context_builder_backend_store_consistency.pas && ./tmp/test_context_builder_backend_store_consistency`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation`
- `python3 -u scripts/compile_all_modules.py`

**Step 2: Check formatting**
- `git diff --check -- src/fafafa.ssl.context.builder.pas tests/scripts/test_context_builder_material_loading_helper_contract.sh docs/plans/2026-03-10-builder-material-loading-helper-extraction.md`
