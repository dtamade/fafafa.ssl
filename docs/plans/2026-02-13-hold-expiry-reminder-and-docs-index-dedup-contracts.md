# Hold Expiry Reminder + Docs Index Dedup Contracts Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add contract tests + minimal path normalization so `scripts/remind_hold_expiry_review_draft.sh` and `scripts/check_docs_index_dedup_draft.sh` work from `/tmp` with repo-relative inputs/outputs, and `--strict` exits non-zero while still producing reports.

**Architecture:** For each script, write a focused `tests/scripts/*.sh` contract test that:
1) executes from repo root with repo-relative paths
2) executes from `/tmp` with the same repo-relative paths
3) asserts report files are written under `$PROJECT_ROOT` for relative `--output`
4) asserts strict-mode exit code behavior (non-zero) and report still exists
Then implement minimal `resolve_input_path` / `resolve_input_dir` / `resolve_output_path` helpers in the scripts.

**Tech Stack:** Bash, coreutils, Python (fixtures only).

---

### Task 1: `remind_hold_expiry_review_draft.sh` path + strict contract

**Files:**
- Create: `tests/scripts/test_remind_hold_expiry_review_path_and_strict_contract.sh`
- Modify: `scripts/remind_hold_expiry_review_draft.sh`

**Step 1: Write the failing test (RED)**
- Create fixture artifact root under `tmp/` with a `.hold.meta` containing an overdue `expires_on`.
- Run from repo root and `/tmp` with:
  - `--root <repo-relative>`
  - `--today 2026-02-13`
  - `--days 7`
  - `--output <repo-relative>`
- Assert report exists under repo root and contains `| overdue | 1 |`.
- Strict: run with `--strict` and assert exit is non-zero while report still exists.

**Step 2: Run test to verify it fails**
Run:
```bash
bash tests/scripts/test_remind_hold_expiry_review_path_and_strict_contract.sh
```

**Step 3: Minimal implementation (GREEN)**
- Add `resolve_input_dir()` for `--root` and normalize `ARTIFACT_ROOT`.
- Add `resolve_output_path()` for `--output` and normalize `OUTPUT_FILE` (after defaults set).

**Step 4: Run test to verify it passes**
Run:
```bash
bash tests/scripts/test_remind_hold_expiry_review_path_and_strict_contract.sh
```

---

### Task 2: `check_docs_index_dedup_draft.sh` path + strict contract

**Files:**
- Create: `tests/scripts/test_check_docs_index_dedup_path_and_strict_contract.sh`
- Modify: `scripts/check_docs_index_dedup_draft.sh`

**Step 1: Write the failing test (RED)**
- Create a fixture index markdown under `tmp/` with duplicated links.
- Run from repo root and `/tmp` with:
  - `--index <repo-relative>`
  - `--scope all`
  - `--output <repo-relative>`
- Assert report exists under repo root.
- Strict: run with `--strict` and assert exit non-zero while report still exists.

**Step 2: Run test to verify it fails**
Run:
```bash
bash tests/scripts/test_check_docs_index_dedup_path_and_strict_contract.sh
```

**Step 3: Minimal implementation (GREEN)**
- Add `resolve_input_path()` for `--index` and normalize `INDEX_FILE`.
- Add `resolve_output_path()` for `--output` and normalize `OUTPUT_FILE`.

**Step 4: Run test to verify it passes**
Run:
```bash
bash tests/scripts/test_check_docs_index_dedup_path_and_strict_contract.sh
```

---

### Task 3: Regression + Evidence

Run:
```bash
for t in tests/scripts/test_*.sh; do echo "==> $t"; bash "$t"; done
```

Update:
- `task_plan.md` (Phase 1W + next queue)
- `findings.md` (root causes + fixes)
- `progress.md` (commands + outputs)

