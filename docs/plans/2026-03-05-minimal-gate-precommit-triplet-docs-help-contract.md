# Minimal Gate Pre-Commit Triplet Option Docs+Help Contract

## Goal
Lock `--with-pre-commit-triplet-contract-batch` visibility in help and contributor docs.

## Architecture
- Add docs/help contract:
  - `tests/scripts/test_minimal_ci_gate_pre_commit_triplet_option_docs_help_contract.sh`
- Contract checks:
  - `scripts/run_minimal_ci_gate.sh --help` includes option token
  - `README.md` includes a quick command using this option
  - `docs/AGENTS.md` includes a quick command using this option
- Integrate into guardrails:
  - `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
  - `tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
  - `tests/fixtures/minimal_ci_gate/contract_batch_scripts.snapshot`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add docs/help contract script
   - make coverage contract require it before batch/snapshot integration
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - expected: fail (missing required entry)
2. GREEN:
   - add script into contract-batch SCRIPTS
   - sync snapshot baseline set
   - add quick command snippets to README + docs/AGENTS
3. Regression:
   - `bash -n tests/scripts/test_minimal_ci_gate_pre_commit_triplet_option_docs_help_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_pre_commit_triplet_option_docs_help_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_pre_commit_triplet_contract_batch_option.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- new triplet option is discoverable in help and docs.
- contract guardrails remain green.
- compile gate remains green (`179/179`, `0 failed`).
