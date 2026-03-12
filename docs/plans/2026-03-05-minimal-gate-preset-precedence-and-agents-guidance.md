# Minimal Gate Preset Precedence + AGENTS Guidance

## Goal
Close `refresh-25` by formalizing preset precedence semantics in minimal CI gate and syncing quick-mode guidance into `docs/AGENTS.md`.

## Architecture
- Contract-first (`tests/scripts/*.sh`):
  - Add a preset precedence contract for mixed usage of:
    - `--fast-local`
    - `--only-platform-path-check-dryrun`
    - `--only-tls13-sign-bench`
  - Pin `last-flag-wins` semantics for preset combinations.
  - Ensure `only-platform` isolation also suppresses runtime OpenSSL cache regression.
- Minimal gate implementation (`scripts/run_minimal_ci_gate.sh`):
  - align `--only-platform-path-check-dryrun` with strict only semantics.
- Developer guidance (`docs/AGENTS.md`):
  - add minimal gate quick commands to reduce option-memory overhead.

## Scope
- Modify: `scripts/run_minimal_ci_gate.sh`
- Modify: `docs/AGENTS.md`
- Add: `tests/scripts/test_minimal_ci_gate_preset_precedence_last_flag_wins_contract.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add preset precedence contract
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_preset_precedence_last_flag_wins_contract.sh`
   - expected: fail (only-platform still allows runtime cache regression command after explicit opt-in)
2. GREEN:
   - update `--only-platform-path-check-dryrun` branch in `scripts/run_minimal_ci_gate.sh`
   - enforce full only isolation for runtime cache regression toggle
3. Regression:
   - `bash -n scripts/run_minimal_ci_gate.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_preset_precedence_last_flag_wins_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_preset_precedence_last_flag_wins_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh`
   - `bash tests/scripts/test_minimal_ci_gate_fast_local_preset_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_only_tls13_sign_bench_mode.sh`
   - `bash tests/scripts/test_minimal_ci_gate_warning_noise_governance_batch_option.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- mixed preset combinations are deterministic with `last-flag-wins` behavior.
- `--only-platform-path-check-dryrun` no longer leaks unrelated runtime regression step.
- developer command guidance in `docs/AGENTS.md` includes minimal gate quick presets.
