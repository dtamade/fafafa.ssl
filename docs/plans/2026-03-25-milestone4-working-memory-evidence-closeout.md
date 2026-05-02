# Milestone 4 Working Memory and Evidence Closeout

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Convert the current working memory from a long list of completed families into milestone-level closeout tracking so future work reopens lines only from fresh failing evidence.

**Architecture:** Keep the detailed family logs intact, but add a repo-wide closeout ledger at the top of `task_plan.md`, `findings.md`, and `progress.md`. The ledger should summarize milestone status, frozen lines, and reopen rules while preserving the existing batch evidence below.

**Tech Stack:** Markdown working memory, roadmap closeout bookkeeping, diff hygiene

## Files
- Add: `docs/plans/2026-03-25-milestone4-working-memory-evidence-closeout.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Steps
1. Re-read the roadmap and current working-memory headers to identify the remaining Milestone 4 gap: the repo still records family-level history, but not a concise milestone-level closeout ledger.
2. Add a top-level ledger to `task_plan.md` that records:
   - Milestone 1 frozen/guarded
   - Milestone 2 currently closed unless a fresh cert-utils RED appears
   - Milestone 3 frozen on the current worktree
   - Milestone 4 in-progress/closed through this ledger update
3. Add matching top-level summary sections to `findings.md` and `progress.md` so reopen rules and latest milestone evidence are visible without scrolling through all older families.
4. Verify the new ledger text is present and the changed markdown passes diff hygiene.
5. Record that this batch did not change production code and therefore did not reopen compile-bearing implementation lines after the immediately preceding milestone gates.

## Verification
- `rg -n "Repo-wide Closeout Ledger|Milestone 1|Milestone 2|Milestone 3|Milestone 4|fresh failing contract|frozen on current worktree" docs/plans/2026-03-25-milestone4-working-memory-evidence-closeout.md task_plan.md findings.md progress.md`
- `git diff --check -- docs/plans/2026-03-25-milestone4-working-memory-evidence-closeout.md task_plan.md findings.md progress.md`

## Expected Outcome
- working memory starts with milestone-level status rather than only family-by-family history
- the next queue becomes “new batch requires fresh RED” instead of another open-ended continuation loop
- closed lines are explicitly marked as closed unless a fresh failing contract or baseline regression appears
