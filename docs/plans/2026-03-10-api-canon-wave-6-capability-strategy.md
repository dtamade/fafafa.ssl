# API Canon Wave 6 — Capability Strategy

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 新建 backend capability / fallback strategy 文档，明确 `TSSLBackendCapabilities` 在 Core/Advanced API 中的角色，以及 capability / dependency / unsupported / fallback 的解释规则。

**Architecture:** 这波只做文档与 docs contract，不改生产代码。文档将把 capability 字段解释成 runtime truth，而不是 marketing table；并说明 Core API 不应该强依赖 capability，Advanced API 才消费它做策略与分流。

**Tech Stack:** Markdown docs, shell docs contract.

---

### Task 1: Add failing docs contract

**Files:**
- Create: `tests/scripts/test_api_capability_strategy_doc_contract.sh`

**Step 1: Write contract**
- Require the new doc to contain:
  - `## Core API`
  - `## Advanced API`
  - `TSSLBackendCapabilities`
  - `RequiresExternalLibrary`
  - `SupportsSystemCertStore`
  - `SupportsPKCS11`
  - `unsupported`
  - `fallback`

**Step 2: Run RED**
- Run: `bash tests/scripts/test_api_capability_strategy_doc_contract.sh`
- Expected: FAIL because the doc does not exist yet.

### Task 2: Create capability strategy doc

**Files:**
- Create: `docs/reference/API_CAPABILITY_STRATEGY.md`

**Step 1: Define capability truth model**
- capability fields are runtime truth, not wish-list

**Step 2: Define Core vs Advanced consumption**
- Core API hides backend choice as much as practical
- Advanced API may branch on capability

**Step 3: Define skip/fallback semantics**
- capability
- dependency
- unsupported
- fallback

### Task 3: Verify docs wave

**Files:**
- Test: `tests/scripts/test_api_capability_strategy_doc_contract.sh`

**Step 1: Run contract**
- Run: `bash tests/scripts/test_api_capability_strategy_doc_contract.sh`
- Expected: PASS

**Step 2: Check formatting**
- Run: `git diff --check -- docs/reference/API_CAPABILITY_STRATEGY.md tests/scripts/test_api_capability_strategy_doc_contract.sh docs/plans/2026-03-10-api-canon-wave-6-capability-strategy.md`
- Expected: PASS
