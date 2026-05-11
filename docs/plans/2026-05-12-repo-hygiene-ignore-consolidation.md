# Repo Hygiene And Ignore Consolidation Plan

**Goal:** reduce repository noise by teaching `.gitignore` about nested `tests/**/test_*` executables, then clear safe ignored build-output directories without touching local agent/config folders or archived notes.

**Architecture:** no production code changes. This batch only:

- expands ignore coverage so nested `tests/*/test_*` binaries stop surfacing as untracked files
- removes safe generated output directories that are already ignored
- keeps `archive/`, `.claude/`, `.fusion/`, and `.ace-tool/` out of the cleanup scope
- verifies the tree is still clean after the sweep

**Files:**

- Modify: `.gitignore`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
- Add: `docs/plans/2026-05-12-repo-hygiene-ignore-consolidation.md`

## Task 1: Expand ignore coverage

Update `.gitignore` so nested `tests/**/test_*` executables are treated the same way as top-level test binaries, while keeping source files (`.pas`, `.lpi`, `.lpr`, `.md`, `.sh`, `.txt`) visible.

## Task 2: Clean safe generated outputs

Remove safe ignored output directories that are clearly build artifacts, including:

- `bin/`
- `tests/bin/`
- `tests/lib/`
- `examples/bin/`
- `artifacts/`
- `tmp/`
- `tools/test_audit/bin/`

If other ignored output directories appear in the inventory and are clearly generated, clean them too, but keep the local config/history folders excluded.

## Task 3: Verify and commit

Run:

```bash
git diff --check -- .gitignore task_plan.md findings.md progress.md docs/plans/2026-05-12-repo-hygiene-ignore-consolidation.md
git status --short
```

Expected:

- no untracked test executables remain
- safe build-output directories are gone
- only intended text-file edits remain
- the batch can be committed cleanly
