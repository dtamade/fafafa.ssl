# Archive Cleanup Record + CI Artifact Cleanup Contracts Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add Bash contract tests and minimal path-resolution fixes so `scripts/generate_archive_cleanup_execution_record_draft.sh` and `scripts/cleanup_ci_artifacts_draft.sh` work when executed from `/tmp` with repo-relative paths.

**Architecture:** Introduce/extend `tests/scripts/*.sh` contract tests that run each script from both repo root and `/tmp` using repo-relative paths. Reproduce RED, then implement the smallest `resolve_*` helper in the script to satisfy the contract, then re-run targeted test and full `tests/scripts` regression.

**Tech Stack:** Bash, coreutils (`stat`, `touch`, `find`), ripgrep.

---

## Task 1: Cleanup Execution Record `--output` Path Contract

**Files:**
- Create: `tests/scripts/test_generate_archive_cleanup_execution_record_path.sh`
- Modify: `scripts/generate_archive_cleanup_execution_record_draft.sh`

**Step 1: Write the failing test (RED)**
- Create a test that:
  - runs the script from repo root with `--output tmp/.../record.md` and asserts the file exists under repo root
  - runs the script from `/tmp` with the same relative `--output` and asserts the file is still written under repo root (not under `/tmp`)

**Step 2: Run test to verify it fails**
Run: `bash tests/scripts/test_generate_archive_cleanup_execution_record_path.sh`
Expected: FAIL with a message like `output should be resolved under project root for relative --output`.

**Step 3: Minimal implementation (GREEN)**
- Add `resolve_output_path()` to `scripts/generate_archive_cleanup_execution_record_draft.sh`:
  - if absolute path: keep
  - if relative path: prefix with `$PROJECT_ROOT/`
- Normalize `OUTPUT_FILE` after defaults are applied and before any dry-run prints / mkdir/write.

**Step 4: Run test to verify it passes**
Run: `bash tests/scripts/test_generate_archive_cleanup_execution_record_path.sh`
Expected: PASS.

---

## Task 2: CI Cleanup `--root` Path + Behavior Contract

**Files:**
- Create: `tests/scripts/test_cleanup_ci_artifacts_root_and_behavior_contract.sh`
- Modify: `scripts/cleanup_ci_artifacts_draft.sh`

**Step 1: Write the failing test (RED)**
- Create fixtures under repo root (e.g. `tmp/test_cleanup_ci_artifacts_contract/artifacts/ci/...`) with:
  - one old run eligible for deletion
  - one old run marked hold (`.hold`) that must never be deleted
  - one new run that should be kept
- Run the script from `/tmp` with `--root tmp/test_cleanup_ci_artifacts_contract/artifacts/ci`:
  - in dry-run: assert it finds candidates and does not delete
  - in apply: assert it deletes only the eligible run and keeps the hold run

**Step 2: Run test to verify it fails**
Run: `bash tests/scripts/test_cleanup_ci_artifacts_root_and_behavior_contract.sh`
Expected: FAIL because relative `--root` is resolved relative to `/tmp` (root not found / no candidates).

**Step 3: Minimal implementation (GREEN)**
- Add `resolve_input_dir()` to `scripts/cleanup_ci_artifacts_draft.sh` (same contract as other scripts):
  - if absolute: keep
  - else if directory exists as given: keep
  - else if `$PROJECT_ROOT/<dir>` exists: use that
- Apply it to `ARTIFACT_ROOT` before checking existence and before `find`.

**Step 4: Run test to verify it passes**
Run: `bash tests/scripts/test_cleanup_ci_artifacts_root_and_behavior_contract.sh`
Expected: PASS.

---

## Task 3: Regression + Evidence

**Files:**
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Step 1: Run full scripts contract regression**
Run: `for t in tests/scripts/*.sh; do bash \"$t\"; done`
Expected: All PASS.

**Step 2: Record evidence**
- Append all command outputs to `progress.md`
- Append root-causes + decisions to `findings.md`
- Update `task_plan.md`:
  - add a new phase entry for this batch (date-stamped)
  - mark checkboxes complete
  - update `Next Priority Queue` (remove completed P1 items)

