# Minimal Gate Pre-Commit Help Text Contract

## Goal
Lock `--pre-commit-minimal` help-text wording in `scripts/run_minimal_ci_gate.sh --help` via executable contract.

## Architecture
- Add help-text contract:
  - `tests/scripts/test_minimal_ci_gate_help_pre_commit_minimal_contract.sh`
- Integrate into minimal gate contract batch guardrails:
  - `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
  - `tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
  - `tests/fixtures/minimal_ci_gate/contract_batch_scripts.snapshot`

## Contract Semantics
`--help` output must include:
- option token: `--pre-commit-minimal`
- equivalence note: `fast-local + skip-warning + contract-batch`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add help-text contract script
   - make coverage contract require this script before batch/snapshot integration
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - expected: fail (missing required contract entry)
2. GREEN:
   - add help-text contract into batch `SCRIPTS`
   - sync snapshot baseline set
3. Regression:
   - `bash -n tests/scripts/test_minimal_ci_gate_help_pre_commit_minimal_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_help_pre_commit_minimal_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- pre-commit preset help wording regressions are detected by contract.
- contract-batch coverage + snapshot remain aligned.
- compile gate remains green (`179/179`, `0 failed`).
