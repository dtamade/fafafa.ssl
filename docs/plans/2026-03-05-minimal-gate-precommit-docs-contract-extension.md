# Minimal Gate Pre-Commit Docs Contract Extension

## Goal
Lock `--pre-commit-minimal` quick-command wording in `README.md` and `docs/AGENTS.md` via executable docs contract.

## Architecture
- Add docs contract:
  - `tests/scripts/test_minimal_ci_gate_pre_commit_docs_contract.sh`
- Integrate into minimal gate contract batch guardrails:
  - `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
  - `tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
  - `tests/fixtures/minimal_ci_gate/contract_batch_scripts.snapshot`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add pre-commit docs contract script
   - make coverage contract require this new script before batch/snapshot integration
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - expected: fail (missing required entry)
2. GREEN:
   - add new docs contract into batch `SCRIPTS`
   - sync snapshot baseline set
3. Regression:
   - `bash -n tests/scripts/test_minimal_ci_gate_pre_commit_docs_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_pre_commit_docs_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- docs quick-command for `--pre-commit-minimal` is protected against accidental regression.
- coverage + snapshot guardrails remain aligned and green.
- compile gate remains green (`179/179`, `0 failed`).
