# Minimal Gate Contract Batch + Help Precedence Note

## Goal
Close `refresh-26` by introducing a single batch contract entrypoint for minimal gate key contracts and documenting preset precedence semantics in `run_minimal_ci_gate.sh --help`.

## Architecture
- Contract aggregation:
  - add `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
  - run key minimal gate contracts in one command:
    - warning-noise default/skip
    - warning-noise timing output
    - fast-local preset
    - only-platform preset
    - only-tls13 preset
    - preset precedence (last-flag-wins)
    - help precedence note
- Help semantics:
  - add explicit line in `scripts/run_minimal_ci_gate.sh` usage text:
    - multiple presets are parsed in order
    - later preset overrides earlier preset (`last-flag-wins`)
- TDD contract:
  - add `tests/scripts/test_minimal_ci_gate_help_preset_precedence_note_contract.sh`

## Scope
- Modify: `scripts/run_minimal_ci_gate.sh`
- Add: `tests/scripts/test_minimal_ci_gate_help_preset_precedence_note_contract.sh`
- Add: `tests/scripts/test_minimal_ci_gate_contract_batch.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add help-note contract and run it
   - expected: fail (help missing precedence note)
2. GREEN:
   - add precedence note to `run_minimal_ci_gate.sh --help`
   - add minimal gate contract batch script
3. Regression:
   - `bash -n scripts/run_minimal_ci_gate.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_help_preset_precedence_note_contract.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_minimal_ci_gate_help_preset_precedence_note_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_preset_precedence_last_flag_wins_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_contract_batch.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- `--help` clearly explains preset precedence (`last-flag-wins`).
- one-command minimal gate contract batch is available and green.
- compile gate remains green (`179/179`, `0 failed`).
