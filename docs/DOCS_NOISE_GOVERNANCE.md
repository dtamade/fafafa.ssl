# Docs Noise Governance

## Scope

This repository distinguishes active docs from historical records when checking documentation noise.

- Active docs:
  - `docs/README.md`
  - `docs/AGENTS.md`
  - `docs/guides/**`
  - `docs/reference/**`
  - `docs/zh/**`
  - `docs/testing/**`
- Historical docs (excluded from active-noise scan):
  - `docs/archive/**`
  - `docs/plans/**`
  - `docs/test_reports/**`
  - Directory notes:
    - `docs/plans/README.md`
    - `docs/test_reports/README.md`

## Rules

- Keep active docs free from temporary marker keywords and temporary placeholder wording.
- If a document is a historical record, keep original wording and place it in a historical path.
- Prefer stable, decision-oriented headings in active docs (for example: `Status`, `Guidelines`, `Recommendations`).

## Suggested Scan Command

```bash
rg -n "TODO|TBD|WIP|FIXME|placeholder|占位|待办" \
  docs \
  --glob '!docs/archive/**' \
  --glob '!docs/plans/**' \
  --glob '!docs/test_reports/**' \
  --glob '!docs/DOCS_NOISE_GOVERNANCE.md'
```

## Baseline (2026-02-22)

- Active-scope keyword hits: `0` (with policy file excluded)
- Historical directories still contain legacy markers by design.
