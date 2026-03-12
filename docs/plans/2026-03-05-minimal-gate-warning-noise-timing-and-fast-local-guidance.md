# Minimal Gate Warning-Noise Timing + Fast-Local Guidance

## Goal
Close `refresh-24` queue by adding lightweight warning-noise timing observability and documenting minimal gate quick presets in developer-facing docs.

## Architecture
- Runtime observability (`scripts/run_minimal_ci_gate.sh`):
  - when warning-noise governance batch is enabled, print a non-blocking elapsed line
  - no threshold and no gate decision based on timing
- Guidance (`README.md`):
  - add concise minimal gate quick commands for:
    - `--fast-local`
    - `--fast-local --skip-warning-noise-governance-batch`
    - `--only-platform-path-check-dryrun`
    - `--only-tls13-sign-bench`

## Scope
- Modify: `scripts/run_minimal_ci_gate.sh`
- Modify: `README.md`
- Add: `tests/scripts/test_minimal_ci_gate_warning_noise_timing_output_contract.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add timing-output contract:
     - `tests/scripts/test_minimal_ci_gate_warning_noise_timing_output_contract.sh`
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_warning_noise_timing_output_contract.sh`
   - expected: fail (timing line absent)
2. GREEN:
   - implement warning-noise elapsed output in `scripts/run_minimal_ci_gate.sh`
   - update `README.md` with minimal gate quick preset section
3. Regression:
   - `bash -n scripts/run_minimal_ci_gate.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_warning_noise_timing_output_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_warning_noise_timing_output_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_fast_local_preset_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_warning_noise_governance_batch_option.sh`
   - `bash tests/scripts/test_minimal_ci_gate_only_tls13_sign_bench_mode.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- warning-noise governance enabled path prints:
  - `[INFO] warning-noise governance elapsed_ms=<N>`
- skip path does not print the timing line.
- minimal gate contracts remain green and compile gate remains green.
