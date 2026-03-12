# API Canon Wave 4 — Entrypoint Governance

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 新建入口治理文档，明确哪些入口是推荐主入口、哪些只保留为兼容入口、哪些属于 deprecated/bridge surface。

**Architecture:** 这波只做文档与 docs contract，不改生产代码。文档将把 `TSSLContextBuilder`、`TSSLFactory + TSSLConfig`、`TSSLConnector / TSSLStream`、`ISSLContext.ServerName`、WolfSSL shim 等入口按治理层次固定下来。

**Tech Stack:** Markdown docs, shell docs contract.

---

### Task 1: Add failing docs contract

**Files:**
- Create: `tests/scripts/test_api_entrypoint_governance_doc_contract.sh`

**Step 1: Write contract**
- Require the doc to contain:
  - 推荐主入口
  - 兼容/底层入口
  - deprecated/bridge surface
  - `TSSLContextBuilder`
  - `TSSLFactory + TSSLConfig`
  - `TSSLConnector / TSSLStream`
  - `ISSLContext.ServerName`
  - WolfSSL shim

**Step 2: Run RED**
- Run: `bash tests/scripts/test_api_entrypoint_governance_doc_contract.sh`
- Expected: FAIL because the doc does not exist yet.

### Task 2: Create entrypoint governance doc

**Files:**
- Create: `docs/reference/API_ENTRYPOINT_GOVERNANCE.md`

**Step 1: Split entry surfaces by governance level**
- 推荐主入口
- 兼容/底层入口
- deprecated/bridge surface

**Step 2: Document the cleanup strategy**
- which surfaces to keep
- which to stop promoting
- which can later be deprecated more strongly

### Task 3: Verify docs wave

**Files:**
- Test: `tests/scripts/test_api_entrypoint_governance_doc_contract.sh`

**Step 1: Run contract**
- Run: `bash tests/scripts/test_api_entrypoint_governance_doc_contract.sh`
- Expected: PASS

**Step 2: Check formatting**
- Run: `git diff --check -- docs/reference/API_ENTRYPOINT_GOVERNANCE.md tests/scripts/test_api_entrypoint_governance_doc_contract.sh docs/plans/2026-03-10-api-canon-wave-4-entrypoint-governance.md`
- Expected: PASS
