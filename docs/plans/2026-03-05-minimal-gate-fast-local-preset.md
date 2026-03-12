# Minimal Gate Fast-Local Preset

## Goal
Provide a single-command fast local verification preset for high-frequency development loops.

## Baseline Evidence
- lightweight comparison (`--skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun`):
  - with warning-noise governance batch: `~5.286s`
  - with `--skip-warning-noise-governance-batch`: `~0.008s`
- conclusion:
  - warning-noise batch is the dominant cost in lightweight mode
  - still acceptable for default-on governance, but ergonomics benefit from a one-flag preset

## Architecture
- Add `--fast-local` to `scripts/run_minimal_ci_gate.sh`
- `--fast-local` preset behavior:
  - disable compile/modules/phase2/platform/docs/purity/tls13-bench/runtime-cache
  - keep warning-noise governance batch enabled by default
  - allow `--skip-warning-noise-governance-batch` override

## Scope
- Modify: `scripts/run_minimal_ci_gate.sh`
- Add: `tests/scripts/test_minimal_ci_gate_fast_local_preset_contract.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add fast-local preset contract
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_fast_local_preset_contract.sh`
   - expected: fail (`Unknown option: --fast-local`)
2. GREEN:
   - implement `--fast-local` parser branch + usage text
3. Regression:
   - `bash -n scripts/run_minimal_ci_gate.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_fast_local_preset_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_fast_local_preset_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_warning_noise_governance_batch_option.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- `--fast-local` dry-run triggers warning-noise governance batch and excludes non-local-heavy steps.
- `--fast-local --skip-warning-noise-governance-batch` skips warning-noise batch.
- compile gate remains green (`179/179`, `0 failed`).
