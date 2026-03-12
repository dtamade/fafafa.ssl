# Minimal Gate Pre-Commit Preset Equivalence Contract

## Goal
Lock behavioral equivalence between:
- `--pre-commit-minimal`
- `--fast-local --skip-warning-noise-governance-batch --with-minimal-gate-contract-batch`

## Architecture
- Add equivalence contract:
  - `tests/scripts/test_minimal_ci_gate_pre_commit_equivalence_contract.sh`
- Integrate into minimal gate contract batch guardrails:
  - `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
  - `tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
  - `tests/fixtures/minimal_ci_gate/contract_batch_scripts.snapshot`

## Contract Semantics
- both command forms must succeed in dry-run mode
- both command forms must produce identical `[GATE]` command sequence
- resulting command sequence must include minimal gate contract batch entrypoint

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add equivalence contract script
   - make coverage contract require this script before batch/snapshot integration
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - expected: fail (missing required entry)
2. GREEN:
   - add equivalence contract into batch `SCRIPTS`
   - sync snapshot baseline set
3. Regression:
   - `bash -n tests/scripts/test_minimal_ci_gate_pre_commit_equivalence_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_pre_commit_equivalence_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- pre-commit shorthand preset remains behaviorally equal to explicit 3-flag command.
- contract-batch coverage + snapshot remain aligned.
- compile gate remains green (`179/179`, `0 failed`).
