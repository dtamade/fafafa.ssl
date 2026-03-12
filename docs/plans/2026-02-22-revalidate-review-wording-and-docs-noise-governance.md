# 2026-02-22 Revalidate/Review Wording + Docs Noise Governance

## Goal

- Align `--strict` usage wording with current runtime semantics in `revalidate/review` scripts.
- Continue docs noise governance in `docs/**` with active-vs-historical scope boundaries.

## Architecture / Scope

- Script changes are documentation-only (help text), no behavior logic changes.
- Docs governance focuses on active docs cleanup and a stable scan policy.
- Exclude historical record directories from active-noise scans:
  - `docs/archive/**`
  - `docs/plans/**`
  - `docs/test_reports/**`

## Files

- `scripts/revalidate_closure_gate_after_autofix_draft.sh`
- `scripts/review_closure_gate_weekly_trend_drift_draft.sh`
- `docs/guides/CODING_STANDARDS.md`
- `docs/reference/OPENSSL_MODULES.md`
- `docs/README.md`
- `docs/DOCS_NOISE_GOVERNANCE.md`

## Step-by-step Commands

1. Baseline scan (active docs scope):
   - `rg -n "TODO|TBD|WIP|FIXME|placeholder|占位|待办" docs --glob '!docs/archive/**' --glob '!docs/plans/**' --glob '!docs/test_reports/**'`
   - Expected: only a small set of active docs hits.
2. Update script usage wording:
   - `revalidate`: strict means `revalidation_status != pass` exits non-zero.
   - `review`: strict means `drift_percent >= drift_threshold` exits non-zero.
3. Clean active-doc marker noise:
   - Replace temporary-marker examples in active guides/reference docs with neutral wording.
4. Add/refresh governance docs:
   - Add `docs/DOCS_NOISE_GOVERNANCE.md`.
   - Link governance and scope boundary from `docs/README.md`.
5. Verification:
   - `bash -n scripts/revalidate_closure_gate_after_autofix_draft.sh`
   - `bash -n scripts/review_closure_gate_weekly_trend_drift_draft.sh`
   - `bash tests/scripts/test_revalidate_closure_gate_after_autofix_strict_contract.sh`
   - `bash tests/scripts/test_review_closure_gate_weekly_trend_drift_strict_contract.sh`
   - `rg -n "TODO|TBD|WIP|FIXME|placeholder|占位|待办" docs --glob '!docs/archive/**' --glob '!docs/plans/**' --glob '!docs/test_reports/**' --glob '!docs/DOCS_NOISE_GOVERNANCE.md'`

## Expected Outputs

- `--help` wording and strict runtime semantics are consistent for both scripts.
- Active docs scope has no marker-keyword hits after cleanup (policy file excluded by design).
- Existing strict contracts remain green.
