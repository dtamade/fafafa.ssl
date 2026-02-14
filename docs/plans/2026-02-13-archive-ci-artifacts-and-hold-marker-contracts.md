# Archive CI Artifacts + Hold Marker Contracts Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add Bash contract tests and minimal path normalization fixes so `scripts/archive_ci_artifacts_draft.sh` and `scripts/mark_ci_artifact_hold_draft.sh` behave deterministically when invoked from `/tmp` with repo-relative paths.

**Architecture:** Treat these scripts as CLI tools with a “repo-relative path contract”. Write contract tests under `tests/scripts/` that run from both repo root and `/tmp`. Observe RED first. Then implement the smallest `resolve_*` helpers in the scripts (absolute stays absolute; relative output paths always resolve under `$PROJECT_ROOT`). Finish with full `tests/scripts/test_*.sh` regression and write evidence into `task_plan.md` / `findings.md` / `progress.md`.

**Tech Stack:** Bash, coreutils, `rg`, Python (test-only helper for setting directory mtimes).

---

### Task 1: `archive_ci_artifacts_draft.sh` output-root path contract

**Files:**
- Create: `tests/scripts/test_archive_ci_artifacts_output_root_contract.sh`
- Modify: `scripts/archive_ci_artifacts_draft.sh`

**Step 1: Write the failing test (RED)**
- Create a test that runs the script from `/tmp` using:
  - `--output-root tmp/<contract>/artifacts/ci_out`
  - `--run-id archive_ci_contract_run`
  - `--profile pr`
  - `--dry-run`
- Assert: stdout line `output_root:` points to `$PROJECT_ROOT/tmp/<contract>/...` (not a caller-relative path).

**Step 2: Run test to verify it fails**
Run:
```bash
bash tests/scripts/test_archive_ci_artifacts_output_root_contract.sh
```
Expected: FAIL because `--output-root` relative path is currently interpreted relative to caller CWD.

**Step 3: Minimal implementation (GREEN)**
- Add `resolve_output_dir()` (same semantics as other scripts’ `resolve_output_path`).
- Apply it to `OUTPUT_ROOT` after parsing CLI options and before computing `RUN_DIR` / `ARCHIVE_FILE`.

**Step 4: Run test to verify it passes**
Run:
```bash
bash tests/scripts/test_archive_ci_artifacts_output_root_contract.sh
```
Expected: PASS.

---

### Task 2: `mark_ci_artifact_hold_draft.sh` root/run-dir path + behavior contract

**Files:**
- Create: `tests/scripts/test_mark_ci_artifact_hold_root_and_behavior_contract.sh`
- Modify: `scripts/mark_ci_artifact_hold_draft.sh`

**Step 1: Write the failing test (RED)**
- Arrange: create `tmp/<contract>/artifacts/ci/<run_id>/` under repo root.
- From `/tmp`, run:
  - `--root tmp/<contract>/artifacts/ci --run-id <run_id> --dry-run` and assert no `.hold` is created.
  - `--root tmp/<contract>/artifacts/ci --run-id <run_id> --apply` and assert `.hold` + `.hold.meta` exist.
  - `--root tmp/<contract>/artifacts/ci --run-id <run_id> --clear --apply` and assert both files are removed.
- Also exercise `--run-dir` with a repo-relative path from `/tmp` (same directory).

**Step 2: Run test to verify it fails**
Run:
```bash
bash tests/scripts/test_mark_ci_artifact_hold_root_and_behavior_contract.sh
```
Expected: FAIL because relative `--root` / `--run-dir` are currently resolved relative to caller CWD.

**Step 3: Minimal implementation (GREEN)**
- Add `resolve_input_dir()` (same semantics as prior cleanup scripts).
- Normalize `ARTIFACT_ROOT` after parsing.
- Normalize `RUN_DIR` when user supplies `--run-dir` (before existence check).

**Step 4: Run test to verify it passes**
Run:
```bash
bash tests/scripts/test_mark_ci_artifact_hold_root_and_behavior_contract.sh
```
Expected: PASS.

---

### Task 3: Regression + Evidence

**Files:**
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Step 1: Run full scripts contract regression**
Run:
```bash
for t in tests/scripts/test_*.sh; do echo "==> $t"; bash "$t"; done
```
Expected: All PASS.

**Step 2: Update planning files**
- Append commands + outputs into `progress.md`
- Append root causes + changes into `findings.md`
- Add a new phase in `task_plan.md` (Phase 1V) and update next priority queue.

