# Minimal Gate Pre-Commit Triplet Contract Batch

## Goal
Add a dedicated pre-commit triplet contract batch entrypoint to reduce local command fragmentation.

## Architecture
- Add triplet batch script:
  - `tests/scripts/test_minimal_ci_gate_pre_commit_triplet_contract_batch.sh`
- Triplet scope:
  - `tests/scripts/test_minimal_ci_gate_pre_commit_minimal_preset_contract.sh`
  - `tests/scripts/test_minimal_ci_gate_pre_commit_docs_contract.sh`
  - `tests/scripts/test_minimal_ci_gate_help_pre_commit_minimal_contract.sh`
- Integrate into minimal gate contract guardrails:
  - `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
  - `tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
  - `tests/fixtures/minimal_ci_gate/contract_batch_scripts.snapshot`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add triplet batch script
   - make coverage contract require this new script before batch/snapshot integration
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - expected: fail (missing required entry)
2. GREEN:
   - add triplet script into contract-batch `SCRIPTS`
   - sync snapshot baseline set
3. Regression:
   - `bash -n tests/scripts/test_minimal_ci_gate_pre_commit_triplet_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_pre_commit_triplet_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- one-command pre-commit triplet contract entrypoint is available.
- coverage + snapshot guardrails stay aligned.
- compile gate remains green (`179/179`, `0 failed`).
