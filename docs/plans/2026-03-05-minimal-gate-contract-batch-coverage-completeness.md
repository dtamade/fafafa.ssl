# Minimal Gate Contract Batch Coverage Completeness

## Goal
Add a coverage-completeness contract to prevent accidental omission of key subcontracts from `tests/scripts/test_minimal_ci_gate_contract_batch.sh`.

## Architecture
- Add new contract:
  - `tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
- Contract checks:
  - key subcontract scripts are present in batch `SCRIPTS` list
  - each key script appears exactly once
  - batch list has no duplicate entries
- Integrate coverage contract into batch execution list so it runs in normal batch path.

## Scope
- Add: `tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
- Modify: `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add coverage contract requiring itself to be part of batch list
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - expected: fail (batch list does not include coverage contract yet)
2. GREEN:
   - append coverage contract path to `SCRIPTS` in batch script
3. Regression:
   - `bash -n tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- minimal gate contract batch includes fixed key subcontract set.
- omission/duplication in batch list becomes contract-visible.
- compile gate remains green (`179/179`, `0 failed`).
