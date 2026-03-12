# API Canon Wave 2 — Contract Index

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 新建当前 API contract index，把已经固定下来的 Core / Advanced / Backend-Specific contract 家族集中成一个可导航的当前真相入口。

**Architecture:** 这波仍以文档与 docs contract 为主，不改生产代码。新文档会把当前 contract 家族按层次分组，明确“推荐先读什么”、“哪些是 Core contract”、“哪些是高级 contract”、“哪些属于 backend-specific strategy/policy”。

**Tech Stack:** Markdown docs, shell docs contract, file-based planning docs.

---

### Task 1: Add failing contract for the contract index doc

**Files:**
- Create: `tests/scripts/test_api_contract_current_index_doc_contract.sh`

**Step 1: Write docs contract**
- Require the new doc to contain:
  - `## Core API Contracts`
  - `## Advanced API Contracts`
  - `## Backend-Specific Contracts`
  - backend resolution
  - config scope boundaries
  - ServerName precedence
  - file/PEM/PKCS11 precedence
  - pure Pascal backend positioning

**Step 2: Run RED**
- Run: `bash tests/scripts/test_api_contract_current_index_doc_contract.sh`
- Expected: FAIL because the new doc does not exist yet.

### Task 2: Create the contract index doc

**Files:**
- Create: `docs/reference/API_CONTRACT_CURRENT_INDEX.md`

**Step 1: Add navigation guidance**
- Explain which docs are current truth:
  - roadmap
  - architecture
  - current summary

**Step 2: Group current contracts**
- Core API
- Advanced API
- Backend-Specific

**Step 3: Link to the key plan/doc sources**
- Prefer current, high-signal plan files rather than raw history dumps.

### Task 3: Verify docs wave

**Files:**
- Test: `tests/scripts/test_api_contract_current_index_doc_contract.sh`

**Step 1: Run contract**
- Run: `bash tests/scripts/test_api_contract_current_index_doc_contract.sh`
- Expected: PASS

**Step 2: Check formatting**
- Run: `git diff --check -- docs/reference/API_CONTRACT_CURRENT_INDEX.md tests/scripts/test_api_contract_current_index_doc_contract.sh docs/plans/2026-03-10-api-canon-wave-2-contract-index.md`
- Expected: PASS

### Task 4: Sync working memory

**Files:**
- Modify: `docs/plans/2026-03-current-summary.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the wave**
- Add RED/GREEN summary, verification commands, and next queue.

**Step 2: Set next wave**
- Next queue becomes pure Pascal client M1 contract decomposition.
