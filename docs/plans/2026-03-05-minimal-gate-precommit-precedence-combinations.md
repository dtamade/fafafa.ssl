# Minimal Gate Pre-Commit Preset Precedence Combinations

## Goal
Add an executable precedence contract for `--pre-commit-minimal` mixed with `--only-*` and `--with-minimal-gate-contract-batch`, enforcing deterministic last-flag-wins behavior.

## Architecture
- Add precedence contract:
  - `tests/scripts/test_minimal_ci_gate_pre_commit_preset_precedence_contract.sh`
- Integrate into minimal gate contract batch guardrails:
  - `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
  - `tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
  - `tests/fixtures/minimal_ci_gate/contract_batch_scripts.snapshot`

## Contract Matrix
- `--pre-commit-minimal --only-platform-path-check-dryrun` -> platform batch on, contract batch off
- `--only-platform-path-check-dryrun --pre-commit-minimal` -> contract batch on, platform batch off
- `--pre-commit-minimal --only-tls13-sign-bench` -> tls13 bench on, contract batch off
- `--only-tls13-sign-bench --pre-commit-minimal` -> contract batch on, tls13 bench off
- `--pre-commit-minimal --fast-local` -> warning-noise on, contract batch off
- `--pre-commit-minimal --only-platform-path-check-dryrun --with-minimal-gate-contract-batch` -> platform batch + contract batch both on

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add precedence contract script
   - make coverage contract require this new script before batch/snapshot integration
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - expected: fail (missing required contract entry)
2. GREEN:
   - add new script into batch `SCRIPTS`
   - sync snapshot baseline set
3. Regression:
   - `bash -n tests/scripts/test_minimal_ci_gate_pre_commit_preset_precedence_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_pre_commit_preset_precedence_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- `--pre-commit-minimal` mixed combinations are deterministic and contract-guarded.
- contract-batch coverage + snapshot guardrails stay green.
- compile gate remains green (`179/179`, `0 failed`).
