# Warning-Noise Governance Batch Gate Integration

## Goal
Integrate `warning-noise governance` batch into the unified local gate entrypoint so developers can opt-in with one switch and reduce manual omission risk.

## Architecture
- Extend `scripts/run_minimal_ci_gate.sh` with a new option:
  - `--with-warning-noise-governance-batch`
- Keep default behavior unchanged:
  - no warning-noise governance batch unless explicitly enabled
- Add a contract test to lock option behavior:
  - default dry-run excludes the batch command
  - opt-in dry-run includes the batch command

## Scope
- Modify: `scripts/run_minimal_ci_gate.sh`
- Add: `tests/scripts/test_minimal_ci_gate_warning_noise_governance_batch_option.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add the new option contract test
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_warning_noise_governance_batch_option.sh`
   - expected: fail (`Unknown option: --with-warning-noise-governance-batch`)
2. GREEN:
   - implement option parse + gated command execution in `run_minimal_ci_gate.sh`
3. Regression:
   - `bash -n scripts/run_minimal_ci_gate.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_warning_noise_governance_batch_option.sh`
   - `bash tests/scripts/test_minimal_ci_gate_warning_noise_governance_batch_option.sh`
   - `bash tests/scripts/test_minimal_ci_gate_docs_governance_batch_option.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- `run_minimal_ci_gate.sh` accepts `--with-warning-noise-governance-batch`.
- Dry-run output includes:
  - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
- Existing minimal gate options and defaults remain stable.
- Module compile gate remains green (`179/179`, `0 failed`).
