# Minimal Gate Contract Batch Set Snapshot Baseline

## Goal
Add a set-snapshot baseline contract for minimal gate contract batch so contract-set changes require explicit baseline updates.

## Architecture
- Add snapshot baseline file:
  - `tests/fixtures/minimal_ci_gate/contract_batch_scripts.snapshot`
- Add snapshot contract:
  - `tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
- Contract behavior:
  - parse `SCRIPTS` list from `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
  - compare set (sorted) against snapshot baseline
  - fail with diff when changed
- Integrate contract into batch list and coverage completeness required list.

## Scope
- Add: `tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
- Add: `tests/fixtures/minimal_ci_gate/contract_batch_scripts.snapshot`
- Modify: `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
- Modify: `tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add snapshot contract (expects snapshot file)
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
   - expected: fail (snapshot baseline missing)
2. GREEN:
   - add snapshot baseline file with current set
   - integrate snapshot contract into batch list
   - update coverage completeness required list
3. Regression:
   - `bash -n tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- minimal gate contract set changes are blocked unless snapshot baseline is explicitly updated.
- coverage + snapshot contracts both run in batch path.
- compile gate remains green (`179/179`, `0 failed`).
