# Repo Review And Targeted Fix Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 用 fresh baseline evidence 审查当前 worktree，锁定一个稳定可复现的问题，并以最小改动完成修复与回归验证。

**Architecture:** 先执行仓库推荐门禁命令获取当前树上的真实失败，再沿 `review -> root cause -> failing test -> minimal fix -> focused + baseline verification` 路径推进。由于 worktree 已有大量用户改动，本批只触碰与 fresh failure 直接相关的最小文件集合，并把证据写回 `task_plan.md`、`findings.md`、`progress.md`。

**Tech Stack:** Free Pascal, Python helper scripts, Bash contract scripts, working-memory markdown files.

---

### Task 1: Establish fresh baseline evidence

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Capture current worktree and review scope**

Run: `git status --short`
Expected:
- large dirty worktree is visible
- no files are reverted
- this batch scope stays limited to fresh reproducible failures only

**Step 2: Run repository compile gate**

Run: `python3 scripts/compile_all_modules.py`
Expected:
- either full pass with current file count
- or a concrete compile failure with file path / error message that can drive the next step

**Step 3: Run local minimal gate**

Run: `bash scripts/run_minimal_ci_gate.sh --fast-local`
Expected:
- either full pass
- or a stable failing stage that narrows the first repair target

**Step 4: Update working memory with evidence**

Expected:
- `task_plan.md` records the active batch and next queue
- `findings.md` captures the first stable failure surface
- `progress.md` logs command evidence and exit status

### Task 2: Investigate the first stable failure

**Files:**
- Modify: `findings.md`
- Modify: `progress.md`
- Modify: only files directly implicated by the failing path

**Step 1: Read the failing file path and related symbols**

Run: use code search plus targeted file reads for the failing unit/script
Expected:
- root-cause hypothesis written down before any code change

**Step 2: Find or add the smallest failing regression**

Run:
- focused test compile/run command for Pascal failures, or
- existing shell contract for script/document drift failures
Expected:
- RED is observed for the specific defect

### Task 3: Implement the minimal fix and verify

**Files:**
- Modify: the minimal production/test files required by the failure
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Apply the smallest safe fix**

Expected:
- only root-cause files are edited
- no unrelated refactor is bundled

**Step 2: Re-run focused verification**

Run: the exact focused command that previously failed
Expected:
- PASS for the repaired behavior

**Step 3: Re-run baseline confidence checks**

Run:
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local` when practical for the touched area
Expected:
- no regression introduced by the fix

### Task 4: Close out with evidence

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record outcome and residual risks**

Expected:
- fixed issue is summarized with file paths and verification evidence
- remaining risks or skipped checks are called out explicitly
