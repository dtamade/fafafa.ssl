# Minimal Gate Contract Batch Option

## Goal
Integrate minimal gate contract batch into `run_minimal_ci_gate.sh` as an explicit opt-in step, keeping default runtime unchanged.

## Architecture
- Add optional switch in `scripts/run_minimal_ci_gate.sh`:
  - `--with-minimal-gate-contract-batch`
- Default behavior:
  - do not run minimal gate contract batch unless explicitly enabled
- Preserve only-mode semantics:
  - `--only-platform-path-check-dryrun` and `--only-tls13-sign-bench` must disable this batch when they appear later
- Contract:
  - `tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`

## Scope
- Modify: `scripts/run_minimal_ci_gate.sh`
- Add: `tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add option contract and run it
   - expected: fail (`Unknown option: --with-minimal-gate-contract-batch`)
2. GREEN:
   - implement option parse + gated command execution
   - enforce only-mode isolation consistency
3. Regression:
   - `bash -n scripts/run_minimal_ci_gate.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch_option.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_help_preset_precedence_note_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_preset_precedence_last_flag_wins_contract.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- default dry-run excludes minimal gate contract batch.
- opt-in dry-run includes minimal gate contract batch.
- only-mode keeps strict isolation semantics unless explicitly overridden later by order.
- compile gate remains green (`179/179`, `0 failed`).
