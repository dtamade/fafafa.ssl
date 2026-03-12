# Minimal Gate Pre-Commit Minimal Preset

## Goal
Add a single preset `--pre-commit-minimal` to reduce command memorization for pre-commit local gate runs.

## Architecture
- Extend minimal gate option parser:
  - `scripts/run_minimal_ci_gate.sh`
- Add preset contract:
  - `tests/scripts/test_minimal_ci_gate_pre_commit_minimal_preset_contract.sh`
- Integrate contract into minimal gate contract batch guardrails:
  - `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
  - `tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
  - `tests/fixtures/minimal_ci_gate/contract_batch_scripts.snapshot`

## Preset Semantics
`--pre-commit-minimal` is equivalent to:
- `--fast-local`
- `--skip-warning-noise-governance-batch`
- `--with-minimal-gate-contract-batch`

So it should:
- include minimal gate contract batch
- skip compile/modules/phase2/platform/docs/warning-noise/tls13/runtime-cache

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add preset contract
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_pre_commit_minimal_preset_contract.sh`
   - expected: fail (`Unknown option: --pre-commit-minimal`)
2. GREEN:
   - implement preset parser branch and help text
   - integrate contract into contract-batch + coverage + snapshot baseline
3. Regression:
   - `bash -n scripts/run_minimal_ci_gate.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_pre_commit_minimal_preset_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_pre_commit_minimal_preset_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- `--pre-commit-minimal` is accepted and stable.
- pre-commit local command no longer depends on memorizing 3 flags.
- minimal gate contract batch guardrails remain complete and green.
