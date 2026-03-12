# Review Remediation Batch 2 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把根目录 `bin/` 从 Git 跟踪中移除，并收敛活跃 GitHub Actions workflow，使默认 CI 面更接近“主线自动 + 定向/手动”的结构。

**Architecture:** 先用脚本契约测试锁定两件事：仓库不应继续跟踪 `bin/*` 产物；定向/草案 workflow 不应继续在每次 push/pull 上自动触发。然后做最小变更：`git rm --cached` 清理跟踪产物，收紧 workflow 触发器并保留主线 Linux CI 与 TLS13 专项 gate。

**Tech Stack:** shell contract tests, Git index hygiene, GitHub Actions YAML.

---

### Task 1: Lock repo hygiene with failing contract test

**Files:**
- Create: `tests/scripts/test_repo_hygiene_no_tracked_root_bin_artifacts.sh`

**Step 1: Write failing contract**
- Assert `git ls-files 'bin/*'` returns no tracked files.

**Step 2: Run test to verify RED**
- Run: `bash tests/scripts/test_repo_hygiene_no_tracked_root_bin_artifacts.sh`
- Expected: FAIL while tracked `bin/*` files still exist.

**Step 3: Remove tracked artifacts from Git index**
- Run: `git rm --cached -r bin`

**Step 4: Re-run test to verify GREEN**
- Run: `bash tests/scripts/test_repo_hygiene_no_tracked_root_bin_artifacts.sh`
- Expected: PASS.

### Task 2: Lock workflow trigger convergence with failing contract test

**Files:**
- Create: `tests/scripts/test_workflow_trigger_convergence_contract.sh`
- Modify: `.github/workflows/ci-matrix-draft.yml`
- Modify: `.github/workflows/phase_c_tests.yml`
- Modify: `.github/workflows/test-all-platforms.yml`

**Step 1: Write failing contract**
- Assert:
  - `ci.yml` remains auto-triggered on push/pull.
  - `phase_c_tests.yml` is manual only.
  - `ci-matrix-draft.yml` is manual only.
  - `test-all-platforms.yml` is nightly/manual, not push/pull.

**Step 2: Run test to verify RED**
- Run: `bash tests/scripts/test_workflow_trigger_convergence_contract.sh`
- Expected: FAIL with current trigger set.

**Step 3: Apply minimal workflow changes**
- Keep `ci.yml` as the default always-on Linux workflow.
- Keep `tls13-signer-gate.yml` as the path-scoped specialized workflow.
- Convert `phase_c_tests.yml` and `ci-matrix-draft.yml` to `workflow_dispatch` only.
- Convert `test-all-platforms.yml` to `schedule` + `workflow_dispatch`.

**Step 4: Re-run test to verify GREEN**
- Run: `bash tests/scripts/test_workflow_trigger_convergence_contract.sh`
- Expected: PASS.

### Task 3: Verify batch outputs

**Files:**
- Verify only

**Step 1: Hygiene checks**
- Run: `git ls-files 'bin/*' | wc -l`
- Expected: `0`.

**Step 2: Contract checks**
- Run both new shell contracts.
- Expected: PASS.
