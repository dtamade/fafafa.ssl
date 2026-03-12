# 2026-02-22 Active Docs Noise Scan Script Contract

## Goal

- Add a single script entrypoint for active docs noise scanning.
- Ensure deterministic behavior when invoked from `/tmp` with repo-relative paths.

## Architecture / Scope

- New script: `scripts/scan_active_docs_noise_draft.sh`
- New contract test: `tests/scripts/test_scan_active_docs_noise_path_and_strict_contract.sh`
- Rules align with `docs/DOCS_NOISE_GOVERNANCE.md`:
  - Exclude `archive/**`, `plans/**`, `test_reports/**`
  - Exclude `DOCS_NOISE_GOVERNANCE.md` by default
- Strict mode exits non-zero when `total_hits > 0`, but report must still be written.

## Files

- `scripts/scan_active_docs_noise_draft.sh`
- `tests/scripts/test_scan_active_docs_noise_path_and_strict_contract.sh`
- `docs/plans/README.md`
- `docs/test_reports/README.md`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_scan_active_docs_noise_path_and_strict_contract.sh`
   - Expected: fail (script missing or behavior not implemented).
2. GREEN:
   - Implement script with:
     - option parsing (`--docs-root`, `--output`, `--strict`, `--include-policy`)
     - path normalization (`$PROJECT_ROOT` fallback for relative paths)
     - markdown report output
     - strict check after report write
3. Regression:
   - `bash tests/scripts/test_scan_active_docs_noise_path_and_strict_contract.sh`
   - `bash tests/scripts/test_scan_active_docs_noise_path_and_strict_contract.sh --strict-check`
   - `bash -n scripts/scan_active_docs_noise_draft.sh`
   - `bash -n tests/scripts/test_scan_active_docs_noise_path_and_strict_contract.sh`
4. Governance follow-up:
   - Add historical-boundary notes for `docs/plans` and `docs/test_reports`.

## Expected Outputs

- Path contract passes: report written under project root from `/tmp`.
- Strict contract passes: non-zero exit with report preserved.
- Active docs noise scan can be run via one script entrypoint.
