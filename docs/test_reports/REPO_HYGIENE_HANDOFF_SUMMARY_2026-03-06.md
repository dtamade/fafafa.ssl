# Repo Hygiene Handoff Summary (2026-03-06)

This handoff groups the recent cleanup work into the four areas that matter most when you pick the repo up later: code, CI, docs, and contracts.

If you want the shortest possible operational entry point, run:

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --pre-commit-minimal
bash tests/scripts/test_repo_hygiene_contract_batch.sh
```

## Code

### OpenSSL naming is more consistent

The OpenSSL backend now has a canonical library-management unit:

- `fafafa.ssl.openssl.lib`

Active Pascal source now prefers canonical OpenSSL API imports:

- `fafafa.ssl.openssl.api.*`

Compatibility wrappers remain in `src/` so older imports still compile when needed, but the repo's active source and current docs no longer treat the old names as the preferred path.

### Two legacy examples were modernized

These examples were updated to compile under the current API surface:

- `examples/test_openssl_rsa.lpr`
- `examples/test_pem.lpr`

The fixes were example-local only. They did not change library behavior.

## CI

### The default workflow now mirrors local verification

`ci.yml` is now the main Linux workflow and follows the same commands people use locally:

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --pre-commit-minimal
```

The broader workflow surface is easier to reason about now:

- `ci.yml` → default Linux path
- `tls13-signer-gate.yml` → targeted path-scoped gate
- `test-all-platforms.yml` → nightly/manual
- `ci-matrix-draft.yml` and `phase_c_tests.yml` → manual only

### Generated artifacts are no longer tracked from `bin/`

Root `bin/*` artifacts were removed from Git tracking. `.gitignore` remains the source of truth for generated outputs.

## Docs

### Current pages now point to the current truth

Current reference docs were updated to prefer canonical OpenSSL API names.

Historical-but-non-archive pages now include a `Historical snapshot` notice and point people to:

- `docs/testing/TESTING_README.md`

This keeps older reports useful as evidence without letting them masquerade as live status dashboards.

### New rollup pages exist

Two pages are useful as entry points:

- `docs/test_reports/REPO_HYGIENE_REMEDIATION_SUMMARY_2026-03-06.md`
- `docs/test_reports/REPO_HYGIENE_HANDOFF_SUMMARY_2026-03-06.md`

## Contracts

### Repo-health protections now have a single batch entry point

Run:

```bash
bash tests/scripts/test_repo_hygiene_contract_batch.sh
```

That batch aggregates the repo-hygiene contracts added during cleanup, including:

- Git artifact tracking checks
- Workflow trigger convergence checks
- Main CI command alignment checks
- OpenSSL naming drift checks
- Legacy example compile checks
- Historical-doc snapshot notice checks

### The batch is itself protected

The batch coverage is guarded by:

- `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`

This makes it harder for the repo-health safety net to silently shrink over time.

## Recommended next steps

If you return to this repo later, use this order:

1. Read `docs/testing/TESTING_README.md`
2. Read `docs/test_reports/REPO_HYGIENE_REMEDIATION_SUMMARY_2026-03-06.md`
3. Run `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
4. Run the main Linux verification path if you need broader confidence

## Known boundaries

A few things are intentionally not fully flattened yet:

- Historical reports still preserve older module names where that context matters.
- The legacy OpenSSL wrapper layer is broader than the active code now needs.
- Some old examples outside the focused modernization set may still need API cleanup later.
