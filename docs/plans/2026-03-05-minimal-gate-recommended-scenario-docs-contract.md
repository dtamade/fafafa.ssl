# Minimal Gate Recommended Scenario Docs Contract

## Goal
Lock `--with-minimal-gate-contract-batch` recommended scenario wording in `README.md` and `docs/AGENTS.md` via executable contract.

## Architecture
- Add docs contract:
  - `tests/scripts/test_minimal_ci_gate_contract_batch_recommended_docs_contract.sh`
- Contract checks:
  - both docs include the recommended pre-commit command
- Integrate into minimal gate contract batch:
  - `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
- Keep coverage + snapshot contracts aligned by updating:
  - `tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
  - `tests/fixtures/minimal_ci_gate/contract_batch_scripts.snapshot`

## Scope
- Add: `tests/scripts/test_minimal_ci_gate_contract_batch_recommended_docs_contract.sh`
- Modify: `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
- Modify: `tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
- Modify: `tests/fixtures/minimal_ci_gate/contract_batch_scripts.snapshot`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - require docs contract path in coverage required set before integration
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - expected: fail (required entry missing from batch)
2. GREEN:
   - add docs contract script
   - append docs contract to batch list
   - update snapshot baseline
3. Regression:
   - `bash -n tests/scripts/test_minimal_ci_gate_contract_batch_recommended_docs_contract.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_recommended_docs_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- docs recommendation command is contract-protected from drift.
- contract batch / coverage / snapshot remain aligned and green.
