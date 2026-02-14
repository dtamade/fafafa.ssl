# Archive Cleanup Record + CI Artifact Cleanup Contracts Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add Bash contract tests and minimal path-resolution fixes so cleanup-related draft scripts behave correctly when invoked from `/tmp` with repo-relative paths.

**Architecture:** Write contract tests under `tests/scripts/` that execute each script from both repo root and `/tmp`, using repo-relative paths for `--output` and `--root`. Observe RED. Implement the smallest path normalization helpers in the scripts (no extra refactors), then re-run the focused test and the full `tests/scripts/*` regression.

**Tech Stack:** Bash, coreutils (`stat`, `touch`), ripgrep (`rg`).

---

### Task 1: Contract Test + Fix for Cleanup Execution Record Output Path

**Files:**
- Create: `tests/scripts/test_generate_archive_cleanup_execution_record_path.sh`
- Modify: `scripts/generate_archive_cleanup_execution_record_draft.sh`

**Step 1: Write the failing test (RED)**

Create `tests/scripts/test_generate_archive_cleanup_execution_record_path.sh`:
- Arrange: pick a stable `--record-id`, and set `--output` to a repo-relative path under `tmp/`.
- Act: run script from repo root (should write under repo root), then run from `/tmp` with the same relative `--output`.
- Assert: output file exists under `$PROJECT_ROOT/<relative>` and contains `record_id`.

**Step 2: Run the test to verify it fails**

Run:
```bash
bash tests/scripts/test_generate_archive_cleanup_execution_record_path.sh
```

Expected (current behavior): FAIL because `/tmp` execution writes to caller CWD rather than `$PROJECT_ROOT`.

**Step 3: Minimal implementation (GREEN)**

Modify `scripts/generate_archive_cleanup_execution_record_draft.sh`:
- Add `resolve_output_path()` (absolute stays absolute; relative becomes `$PROJECT_ROOT/<relative>`).
- Apply it to `OUTPUT_FILE` after defaults are set and before `mkdir -p` / write.

**Step 4: Run the test to verify it passes**

Run:
```bash
bash tests/scripts/test_generate_archive_cleanup_execution_record_path.sh
```

Expected: PASS.

---

### Task 2: Contract Test + Fix for CI Artifact Cleanup Root Path + Safety Behavior

**Files:**
- Create: `tests/scripts/test_cleanup_ci_artifacts_root_and_behavior_contract.sh`
- Modify: `scripts/cleanup_ci_artifacts_draft.sh`

**Step 1: Write the failing test (RED)**

Create `tests/scripts/test_cleanup_ci_artifacts_root_and_behavior_contract.sh`:
- Arrange: create a temp artifact root under `tmp/` with 3 run directories:
  - `run_old_delete` (mtime older than threshold; should become candidate and be deleted only on `--apply`)
  - `run_old_hold` (mtime older; contains `.hold`; should be skipped and never deleted)
  - `run_new_keep` (mtime newer; should be kept)
- Act/Assert (dry-run): run script from `/tmp` with `--root <repo-relative>` and ensure it reports `[CANDIDATE] run_old_delete` and `[SKIP-HOLD] run_old_hold`, and does not delete anything.
- Act/Assert (apply): run script from `/tmp` with `--apply` and ensure `run_old_delete` is removed while `run_old_hold` remains.

**Step 2: Run the test to verify it fails**

Run:
```bash
bash tests/scripts/test_cleanup_ci_artifacts_root_and_behavior_contract.sh
```

Expected (current behavior): FAIL because `/tmp` execution treats `--root <relative>` as relative to `/tmp` and cannot find the repo directory.

**Step 3: Minimal implementation (GREEN)**

Modify `scripts/cleanup_ci_artifacts_draft.sh`:
- Add `resolve_input_dir()` (absolute stays absolute; if `-d <relative>` exists use it; else fallback to `$PROJECT_ROOT/<relative>`).
- Apply it to `ARTIFACT_ROOT` after CLI parsing and before `-d` checks / cleanup loop.

**Step 4: Run the test to verify it passes**

Run:
```bash
bash tests/scripts/test_cleanup_ci_artifacts_root_and_behavior_contract.sh
```

Expected: PASS.

---

### Task 3: Regression

Run:
```bash
for t in tests/scripts/test_*.sh; do echo "==> $t"; bash "$t"; done
```

Expected: all PASS.

---

### Task 4: Update Planning Files (Evidence)

Append to:
- `progress.md`: all commands + outputs for this batch.
- `findings.md`: root cause + fix summary for each script.
- `task_plan.md`: add a new phase entry (e.g., Phase 1U) and update the Next Priority Queue.

