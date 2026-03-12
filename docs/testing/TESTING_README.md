# fafafa.ssl Testing and Validation Guide

**Last updated**: 2026-03-06

This page describes the current verification entry points for `fafafa.ssl`.

If you only need one page, start with `docs/testing/CURRENT_HEALTH.md`. Historical reports in `docs/archive/` and older phase documents are useful context, but they are not a live source of truth for current repository health.

## Start with these local checks

Run the core compile gate:

```bash
python3 scripts/compile_all_modules.py
```

Then run the lightweight local gate:

```bash
bash scripts/run_minimal_ci_gate.sh --fast-local
```

Use `--pre-commit-minimal` when you want a slightly broader pass before sharing changes:

```bash
bash scripts/run_minimal_ci_gate.sh --pre-commit-minimal
```

## What each check tells you

`python3 scripts/compile_all_modules.py`
- Compiles the core Pascal units on Linux.
- Good for catching interface drift, missing dependencies, and build regressions.

`bash scripts/run_minimal_ci_gate.sh --fast-local`
- Runs the fastest stable local contract batch.
- Good for day-to-day iteration when you want feedback quickly.

`bash scripts/run_minimal_ci_gate.sh --pre-commit-minimal`
- Adds a broader set of contract checks that are useful before review.
- Good for a final local sweep without jumping straight to the largest workflows.

`bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- Runs the repo-health contracts added during the cleanup work.
- Good when you want one command that checks Git hygiene, workflow drift, naming drift, example compile coverage, and historical-doc labeling.

## Where tests live

- `tests/` contains Pascal tests and backend-oriented verification programs.
- `tests/scripts/` contains shell contract tests for scripts in `scripts/`.
- `tests/framework/` contains shared helpers used by Pascal tests.
- `examples/` contains runnable examples, not the primary source of test truth.

## Read `PASS`, `SKIP`, and `BLOCKED` correctly

Not every non-`PASS` line is a failure.

- `PASS` means the asserted behavior completed successfully.
- `SKIP` means the test recognized an unavailable dependency, capability, or environment and exited intentionally.
- `BLOCKED` is used for platform-specific cases, such as WinSSL checks on non-Windows systems.

For capability semantics and skip policy, check `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`.

## CI scope today

Active workflow files live in `.github/workflows/`.

A few important details:
- `ci.yml` is intended to mirror the two Linux commands the team uses locally: `python3 scripts/compile_all_modules.py` and `bash scripts/run_minimal_ci_gate.sh --fast-local`.
- Some workflows are intentionally narrow and cover only targeted suites.
- Broad cross-platform workflows are better treated as nightly or manual capacity, not the default review signal.
- Some workflow files are currently disabled or draft-like and should not be treated as always-on coverage.
- Local gates remain the most reliable way to verify changes before review.

If you need broader evidence, inspect the specific workflow you care about and match it to the command it runs.

## Historical reports

Use these directories as reference material, not status dashboards:

- `docs/archive/`
- `docs/test_reports/`

Those files explain why certain gates or conventions exist, but their numeric summaries can drift as the repository grows.
