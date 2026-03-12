# API Canon Wave 5 — Error Model

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 新建 API error/result/exception 语义文档，把 Core API 与 Advanced API 的错误暴露规则固定下来。

**Architecture:** 这波只做文档与 docs contract，不改生产代码。文档将明确 `TSSLOperationResult` / `TSSLDataResult` 的位置、异常层级的定位、warning 的 contract 角色，以及 Core API vs Advanced API 如何暴露错误信息。

**Tech Stack:** Markdown docs, shell docs contract.

---

### Task 1: Add failing docs contract

**Files:**
- Create: `tests/scripts/test_api_error_model_doc_contract.sh`

**Step 1: Write contract**
- Require the new doc to contain:
  - `## Core API`
  - `## Advanced API`
  - `TSSLOperationResult`
  - `TSSLDataResult`
  - `ESSLException`
  - `warning`
  - `unsupported`
  - `configuration`

**Step 2: Run RED**
- Run: `bash tests/scripts/test_api_error_model_doc_contract.sh`
- Expected: FAIL because the doc does not exist yet.

### Task 2: Create the error model doc

**Files:**
- Create: `docs/reference/API_ERROR_MODEL.md`

**Step 1: Define Core API error surface**
- stable exceptions
- simple result types
- no backend-specific leakage by default

**Step 2: Define Advanced API error surface**
- richer exception types
- structured unsupported semantics
- capability / warning / diagnostics layers

**Step 3: Define warning contract role**
- warnings must explain precedence, fallback, ambiguity

### Task 3: Verify docs wave

**Files:**
- Test: `tests/scripts/test_api_error_model_doc_contract.sh`

**Step 1: Run contract**
- Run: `bash tests/scripts/test_api_error_model_doc_contract.sh`
- Expected: PASS

**Step 2: Check formatting**
- Run: `git diff --check -- docs/reference/API_ERROR_MODEL.md tests/scripts/test_api_error_model_doc_contract.sh docs/plans/2026-03-10-api-canon-wave-5-error-model.md`
- Expected: PASS
