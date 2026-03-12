# Minimal Gate Contract Batch Recommended Scenarios (Docs)

## Goal
Document when to use `--with-minimal-gate-contract-batch` in contributor-facing quick-command sections.

## Architecture
- Update quick-mode snippets in:
  - `README.md`
  - `docs/AGENTS.md`
- Add explicit recommended scenario:
  - pre-commit local contract regression for minimal gate semantics.

## Scope
- Modify: `README.md`
- Modify: `docs/AGENTS.md`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Add pre-commit command snippet in both docs.
2. Verify both files contain `--with-minimal-gate-contract-batch` in quick-mode sections.

## Verification
- `rg -n "with-minimal-gate-contract-batch" README.md docs/AGENTS.md`

## Expected Outputs
- contributors can discover the option by scenario (pre-commit local contracts), not only by option name.
