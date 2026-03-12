# Review Remediation Batch 3 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让默认 `ci.yml` 直接运行已经在本地验证过的 Linux 命令组合，减少“CI 看起来在测别的、本地在跑另一套”的分叉。

**Architecture:** 先写一个 shell 契约测试，锁定 `ci.yml` 的主验证命令必须是 `python3 scripts/compile_all_modules.py` 和 `bash scripts/run_minimal_ci_gate.sh --pre-commit-minimal`。然后对 workflow 做最小改动：保留 Linux 默认触发，去掉与当前验证主线不一致的命令和多余描述，让 `ci.yml` 成为“本地主线命令的远端映射”。

**Tech Stack:** shell contract tests, GitHub Actions YAML, Python compile gate, minimal CI gate shell script.

---

### Task 1: Lock main CI command contract with a failing test

**Files:**
- Create: `tests/scripts/test_main_ci_workflow_local_verified_commands_contract.sh`
- Modify: `.github/workflows/ci.yml`

**Step 1: Write failing contract**
- Assert `ci.yml` contains:
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --pre-commit-minimal`
- Assert `ci.yml` no longer runs `./scripts/run_all_module_tests.sh --verbose` as the main verification command.

**Step 2: Run test to verify RED**
- Run: `bash tests/scripts/test_main_ci_workflow_local_verified_commands_contract.sh`
- Expected: FAIL with the current workflow.

**Step 3: Apply minimal workflow update**
- Keep push/pull/schedule triggers.
- Replace the main test execution with the compile gate plus `--pre-commit-minimal`.
- Keep the workflow scoped to Linux.

**Step 4: Re-run test to verify GREEN**
- Run the same shell contract.
- Expected: PASS.

### Task 2: Verify the commands used by CI

**Files:**
- Verify only

**Step 1: Run local commands**
- Run: `python3 scripts/compile_all_modules.py`
- Run: `bash scripts/run_minimal_ci_gate.sh --pre-commit-minimal`
- Expected: PASS.

**Step 2: Validate YAML**
- Parse `.github/workflows/ci.yml` successfully.
