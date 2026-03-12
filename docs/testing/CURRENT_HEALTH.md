# Current Health

Use this page when you want the fastest reliable answer to: "Is the repository healthy right now?"

If you only run two commands, run these:

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## What these commands prove

`python3 scripts/compile_all_modules.py`
- Compiles the core Pascal module set on Linux.
- Catches missing units, interface drift, and build regressions in the active source tree.

`bash scripts/run_minimal_ci_gate.sh --fast-local`
- Runs the current fast local smoke path.
- Catches focused compile regressions and warning-noise contract drift without jumping to the broadest workflows.

## What to run before review

When you want the broadest active-program runtime sweep, run:

```bash
bash tests/scripts/test_active_program_runtime_contract_batch.sh
```

This batch exercises the current active Pascal program runtime contracts across integration, certificate, examples, and tool entrypoints.

When you want one more pass before handing work off, run:

```bash
bash scripts/run_minimal_ci_gate.sh --pre-commit-minimal
```

This adds the broader minimal-gate contract batch while staying cheaper than the largest manual or nightly workflows.

For a short summary of the March 7, 2026 runtime-contract cleanup, check `docs/testing/RUNTIME_CONTRACT_CLEANUP_SUMMARY_2026-03-07.md`.
For a PR-ready bilingual version, check `docs/testing/RUNTIME_CONTRACT_CLEANUP_PR_SUMMARY_2026-03-07.md`.

## What main CI runs today

The main Linux workflow mirrors the same local-first story:

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

That means the fastest local smoke path and the default Linux CI path now exercise the same high-signal entry points.

## Read historical docs the right way

The repository has a lot of historical material under `docs/archive/`, `docs/plans/`, and `docs/test_reports/`.

Use those directories for background, rollout history, and audit context. Do not treat them as the live status page for the current tree.

## Related pages

- `docs/testing/TESTING_README.md` for the broader testing map
- `docs/README.md` for the overall docs index
- `tests/scripts/` for shell contract tests
- `.github/workflows/ci.yml` for the main Linux CI definition
