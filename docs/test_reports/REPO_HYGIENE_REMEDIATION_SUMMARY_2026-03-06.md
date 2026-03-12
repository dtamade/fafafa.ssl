# Repo Hygiene Remediation Summary (2026-03-06)

This page summarizes the repo-health cleanup work completed across the recent remediation batches.

Use this page when you want the short version of what changed, why it changed, and what the current default verification path looks like.

## What changed

### 1. Verification now points to one clear local path

The repository previously had multiple overlapping ways to describe project health. The cleanup work aligned the main Linux verification flow around two commands:

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --pre-commit-minimal
```

Those commands now appear consistently in the main workflow, the testing guide, and the README.

### 2. The default CI surface is narrower and easier to trust

The active workflow set was reduced to a clearer shape:

- `ci.yml` is the default Linux workflow.
- `tls13-signer-gate.yml` remains a targeted path-scoped workflow.
- `test-all-platforms.yml` is nightly/manual.
- `ci-matrix-draft.yml` and `phase_c_tests.yml` are manual only.

This reduces the gap between “what looks active” and “what is actually the day-to-day review signal.”

### 3. Generated artifacts are no longer tracked from the root `bin/` directory

Root `bin/*` outputs were removed from Git tracking while leaving `.gitignore` as the source of truth. This cuts review noise and keeps the repository closer to reproducible source state.

### 4. OpenSSL naming drift was reduced in both code and docs

OpenSSL library-management imports now have a canonical entry point:

- `fafafa.ssl.openssl.lib`

Active Pascal source now prefers canonical OpenSSL API imports:

- `fafafa.ssl.openssl.api.*`

Compatibility wrappers remain in place so older imports can still compile where needed, but active source and current reference docs now prefer the canonical names.

### 5. Historical pages are labeled more clearly

Several non-archive testing and validation pages contained fixed historical metrics that could be misread as current status. Those pages now include a `Historical snapshot` banner and point readers to `docs/testing/TESTING_README.md` for the current entry path.

## Current default verification path

If you want the fastest reliable answer to “is this workspace healthy?” start here:

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --pre-commit-minimal
```

If you only want the lightest local sweep during iteration, use:

```bash
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Evidence captured during remediation

The cleanup batches added lightweight contracts so the repo is less likely to drift back.

If you want to run the full repo-hygiene protection set in one command, use:

```bash
bash tests/scripts/test_repo_hygiene_contract_batch.sh
```

Key examples:

- `tests/scripts/test_repo_hygiene_no_tracked_root_bin_artifacts.sh`
- `tests/scripts/test_workflow_trigger_convergence_contract.sh`
- `tests/scripts/test_main_ci_workflow_local_verified_commands_contract.sh`
- `tests/scripts/test_openssl_lib_canonical_imports_contract.sh`
- `tests/scripts/test_legacy_openssl_api_shim_coverage_contract.sh`
- `tests/scripts/test_legacy_openssl_api_canonical_imports_contract.sh`
- `tests/scripts/test_legacy_openssl_examples_compile_contract.sh`
- `tests/scripts/test_historical_snapshot_notice_contract.sh`

## What still remains

The repo is in a much better state than it was before this cleanup, but a few follow-up areas still exist:

- Some old historical reports still contain outdated module names by design.
- Some legacy examples outside the focused modernization set may still need API-level cleanup.
- The compatibility shim layer for legacy OpenSSL unit names is intentionally broad; a future pass could decide whether to keep or reduce that surface.

## Recommended reading

- Handoff checklist: `docs/test_reports/REPO_HYGIENE_HANDOFF_SUMMARY_2026-03-06.md`
- Current verification entry point: `docs/testing/TESTING_README.md`
- Historical reports boundary: `docs/test_reports/README.md`
- Current OpenSSL reference naming: `docs/reference/OPENSSL_MODULES.md`
