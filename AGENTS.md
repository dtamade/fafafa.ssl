# Agent Team Guide (fafafa.ssl)

This repository uses a simple 3-role "agent team" workflow to keep delivery fast and verifiable.

For repository conventions (Pascal layout, build/test commands, style), see `docs/AGENTS.md`.

## Roles

### 1) Implementer (Coder)
- Writes tests first (strict RED -> GREEN -> Regression).
- Makes the smallest safe change to satisfy the test.
- Keeps changes scoped; avoids unrelated refactors.

### 2) Reviewer
- Reviews diffs for correctness, regressions, and contract compatibility.
- Verifies evidence: required commands were run and results recorded.
- Blocks risky changes (behavior drift, missing tests, brittle assumptions).

### 3) Coordinator (Plan Driver)
- Scans repo gaps, sets priority, and writes executable plans.
- Maintains file-based working memory:
  - `task_plan.md` (phases, next queue)
  - `findings.md` (decisions, root causes)
  - `progress.md` (command logs + outputs)
- Drives execution in small batches; stops on blockers and escalates.

## Working Protocol
- Plans live in `docs/plans/YYYY-MM-DD-<topic>.md` and must include:
  - goal, architecture, files, step-by-step commands, expected outputs.
- Execution follows the plan task-by-task (no skipping verifications).
- TDD is mandatory for behavior changes:
  - No production code before a failing test is observed.
- Verification expectations (pick what applies):
  - Bash scripts: contract tests in `tests/scripts/*.sh` + `bash -n scripts/<file>.sh`
  - Pascal core: `python3 scripts/compile_all_modules.py` + relevant focused test runs

## Definition of Done (per batch)
- New behavior is covered by a test (or contract test for scripts).
- Tests/regressions are green (or failures are explained and intentional).
- `task_plan.md` / `findings.md` / `progress.md` updated with evidence.

