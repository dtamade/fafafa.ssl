# API Canon Wave 1 — Architecture Rewrite

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 将 `docs/reference/ARCHITECTURE.md` 重写为当前 SSL/TLS API canon 文档，作为后续路线图执行的设计真相源。

**Architecture:** 这波只做文档与 docs contract，不改生产代码。文档将明确 Core / Advanced / Backend-Specific 三层、主入口/兼容入口关系、builder/factory/config scope contract、backend/support matrix，以及 pure Pascal 后端的战略定位与 M1 验收标准。

**Tech Stack:** Markdown docs, shell docs contract, file-based planning docs.

---

### Task 1: Add failing architecture canon doc contract

**Files:**
- Create: `tests/scripts/test_architecture_api_canon_doc_contract.sh`

**Step 1: Write the failing docs contract**
- Assert the architecture doc contains:
  - `## API Canon`
  - `### Core API`
  - `### Advanced API`
  - `### Backend-Specific API`
  - `TSSLContextBuilder` as main entry
  - `TSSLFactory + TSSLConfig` as compatibility/low-level entry
  - `UsePKCS11(...)` private-key-only semantics
  - pure Pascal backend positioning
  - Linux + Windows platform priority

**Step 2: Run RED**
- Run: `bash tests/scripts/test_architecture_api_canon_doc_contract.sh`
- Expected: FAIL because the old architecture doc is still milestone/history shaped, not API-canon shaped.

### Task 2: Rewrite architecture doc

**Files:**
- Modify: `docs/reference/ARCHITECTURE.md`

**Step 1: Replace the outdated architecture narrative**
- Remove obsolete “week-by-week backend buildout” structure.
- Replace with current-truth sections:
  - purpose/status
  - design priorities
  - target users / platform priorities
  - API canon layers
  - entrypoint governance
  - core behavior contracts
  - backend model and support matrix
  - pure Pascal backend role and M1
  - error/observability direction
  - compatibility policy

**Step 2: Keep current verified truths**
- Preserve and restate current contracts already proven in tests:
  - single backend resolution
  - request/context vs library scope split
  - ServerName precedence
  - certificate/private-key file/PEM precedence
  - PKCS11 precedence and cert-required semantics

### Task 3: Verify docs wave

**Files:**
- Test: `tests/scripts/test_architecture_api_canon_doc_contract.sh`

**Step 1: Run contract**
- Run: `bash tests/scripts/test_architecture_api_canon_doc_contract.sh`
- Expected: PASS

**Step 2: Check formatting**
- Run: `git diff --check -- docs/reference/ARCHITECTURE.md tests/scripts/test_architecture_api_canon_doc_contract.sh docs/plans/2026-03-10-api-canon-wave-1-architecture-rewrite.md`
- Expected: PASS

### Task 4: Sync working memory

**Files:**
- Modify: `docs/plans/2026-03-current-summary.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the wave**
- Add the plan entry, RED/GREEN summary, verification commands, and next queue.

**Step 2: Keep roadmap continuity**
- Mark Wave 1 as complete and set the next queue to API contract index extraction.
