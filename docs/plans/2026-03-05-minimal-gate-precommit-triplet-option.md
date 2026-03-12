# Minimal Gate Pre-Commit Triplet Batch Option

## Goal
Add a minimal gate entry option for pre-commit triplet contract batch, default-off and explicit opt-in.

## Architecture
- Extend minimal gate option parser and gate execution:
  - `scripts/run_minimal_ci_gate.sh`
- Add option contract:
  - `tests/scripts/test_minimal_ci_gate_pre_commit_triplet_contract_batch_option.sh`
- Integrate contract into existing contract guardrails:
  - `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
  - `tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
  - `tests/fixtures/minimal_ci_gate/contract_batch_scripts.snapshot`

## Option Semantics
- new option: `--with-pre-commit-triplet-contract-batch`
- default behavior: do not run triplet batch
- opt-in behavior: run `tests/scripts/test_minimal_ci_gate_pre_commit_triplet_contract_batch.sh`
- precedence: last-flag-wins for mixed presets/options

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add option contract and run it
   - expected: fail (`Unknown option: --with-pre-commit-triplet-contract-batch`)
2. GREEN:
   - implement parser + execution branch in `run_minimal_ci_gate.sh`
   - integrate option contract into contract-batch/coverage/snapshot
3. Regression:
   - `bash -n scripts/run_minimal_ci_gate.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_pre_commit_triplet_contract_batch_option.sh`
   - `bash tests/scripts/test_minimal_ci_gate_pre_commit_triplet_contract_batch_option.sh`
   - `bash tests/scripts/test_minimal_ci_gate_pre_commit_triplet_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- minimal gate supports explicit pre-commit triplet batch opt-in.
- default path remains unchanged.
- contract guardrails remain green.
