# Repo Hygiene Contract Batch Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把最近新增的仓库卫生 contract tests 汇总为一个可执行入口 `tests/scripts/test_repo_hygiene_contract_batch.sh`，方便一次性跑完当前 repo-health 保护网。

**Architecture:** 先写一个 shell 契约测试，锁定 batch 脚本必须存在、必须包含预期的 hygiene contract 列表。然后用现有 batch 脚本风格实现 `test_repo_hygiene_contract_batch.sh`，最后跑契约和 batch 自身确认通过。

**Tech Stack:** shell contract tests, bash batch runner.

---

### Task 1: Lock batch coverage with a failing contract

**Files:**
- Create: `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Create: `tests/scripts/test_repo_hygiene_contract_batch.sh`

**Step 1: Write failing contract**
- Assert batch script exists.
- Assert batch script references the expected repo hygiene contracts.

**Step 2: Run test to verify RED**
- Run: `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Expected: FAIL before the batch script exists.

### Task 2: Add the batch runner

**Files:**
- Create: `tests/scripts/test_repo_hygiene_contract_batch.sh`

**Step 1: Implement batch runner**
- Follow the pattern used by other `*_contract_batch.sh` scripts.
- Run the selected repo hygiene contracts in a fixed order.

**Step 2: Re-run contract**
- Run: `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Expected: PASS.

### Task 3: Verify the batch itself

**Files:**
- Verify only

**Step 1: Run the batch**
- Run: `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- Expected: PASS.
