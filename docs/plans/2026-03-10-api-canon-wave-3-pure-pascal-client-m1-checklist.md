# API Canon Wave 3 — Pure Pascal Client M1 Checklist

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 新建 pure Pascal HTTPS/TLS client M1 checklist，把 10 条 M1 验收标准拆成当前状态：已满足 / 部分满足 / 缺失，并给出证据入口。

**Architecture:** 这波仍然是文档化与 contract/index 工作，不改生产代码。文档会只根据当前已存在的 test/plan/evidence 给状态，不靠主观推断。

**Tech Stack:** Markdown docs, shell docs contract, existing plans/tests as evidence sources.

---

### Task 1: Add failing docs contract

**Files:**
- Create: `tests/scripts/test_pure_pascal_client_m1_checklist_doc_contract.sh`

**Step 1: Write docs contract**
- Require the checklist doc to contain:
  - `## M1 Target`
  - `## 已满足`
  - `## 部分满足`
  - `## 缺失`
  - all 10 M1 capability lines

**Step 2: Run RED**
- Run: `bash tests/scripts/test_pure_pascal_client_m1_checklist_doc_contract.sh`
- Expected: FAIL because the checklist doc does not exist yet.

### Task 2: Create the checklist doc

**Files:**
- Create: `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`

**Step 1: Restate the M1 target**
- Make the target explicit: Linux-first, HTTPS/TLS client production usable.

**Step 2: Classify each M1 item**
- 已满足
- 部分满足
- 缺失

**Step 3: Add evidence links**
- Point each item to the strongest current plan/test/doc evidence.

### Task 3: Verify docs wave

**Files:**
- Test: `tests/scripts/test_pure_pascal_client_m1_checklist_doc_contract.sh`

**Step 1: Run contract**
- Run: `bash tests/scripts/test_pure_pascal_client_m1_checklist_doc_contract.sh`
- Expected: PASS

**Step 2: Check formatting**
- Run: `git diff --check -- docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md tests/scripts/test_pure_pascal_client_m1_checklist_doc_contract.sh docs/plans/2026-03-10-api-canon-wave-3-pure-pascal-client-m1-checklist.md`
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
- Next queue becomes API entry slimming / helper extraction preparation.
