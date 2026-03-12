# 2026-02-24 Minimal CI Gate Docs Governance Strict Batch Option

## Goal

- Add an optional docs-governance strict batch step to `run_minimal_ci_gate.sh`.
- Keep default behavior unchanged (docs governance batch disabled unless explicitly enabled).

## Architecture / Scope

- Script:
  - `scripts/run_minimal_ci_gate.sh`
  - Add option:
    - `--with-docs-governance-strict-batch`
  - Behavior:
    - default: do not invoke docs governance batch
    - enabled: invoke `tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh`
- Contract:
  - `tests/scripts/test_minimal_ci_gate_docs_governance_batch_option.sh`
  - Validate:
    - default dry-run excludes docs governance batch command.
    - opt-in dry-run includes docs governance batch command.

## Files

- `scripts/run_minimal_ci_gate.sh`
- `tests/scripts/test_minimal_ci_gate_docs_governance_batch_option.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_minimal_ci_gate_docs_governance_batch_option.sh`
   - Expected: fail (`Unknown option: --with-docs-governance-strict-batch`).
2. GREEN:
   - Implement option parsing + gated command execution.
3. Regression:
   - `bash tests/scripts/test_minimal_ci_gate_docs_governance_batch_option.sh`
   - `bash tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_docs_governance_integration.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_skip_local_guard_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_local_guard_batch_integration.sh`
   - `bash -n scripts/run_minimal_ci_gate.sh tests/scripts/test_minimal_ci_gate_docs_governance_batch_option.sh`

## Expected Outputs

- Minimal CI gate supports optional docs-governance strict batch checks.
- Existing default flow and platform path-check behavior remain unchanged.
