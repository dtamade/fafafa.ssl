# Minimal Gate Only-TLS13-Bench Only-Mode Contract

## Goal
Ensure `--only-tls13-sign-bench` is a true only-mode path: it should run TLS13 sign bench and avoid unrelated gate steps.

## Architecture
- Add contract test for `run_minimal_ci_gate.sh --only-tls13-sign-bench --dry-run`:
  - must include TLS13 bench command
  - must exclude compile/modules/phase2/platform-path-check/docs-governance/warning-noise/purity/runtime-cache steps
- Align parser branch in `run_minimal_ci_gate.sh` to enforce only semantics.

## Scope
- Add: `tests/scripts/test_minimal_ci_gate_only_tls13_sign_bench_mode.sh`
- Modify: `scripts/run_minimal_ci_gate.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - add only-tls13 mode contract
   - run:
     - `bash tests/scripts/test_minimal_ci_gate_only_tls13_sign_bench_mode.sh`
   - expected: fail if unrelated default steps still run
2. GREEN:
   - tighten `--only-tls13-sign-bench` branch to disable unrelated steps
3. Regression:
   - `bash -n scripts/run_minimal_ci_gate.sh`
   - `bash -n tests/scripts/test_minimal_ci_gate_only_tls13_sign_bench_mode.sh`
   - `bash tests/scripts/test_minimal_ci_gate_only_tls13_sign_bench_mode.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_minimal_ci_gate_warning_noise_governance_batch_option.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- `--only-tls13-sign-bench` dry-run outputs only bench command (plus metadata lines), without unrelated gate steps.
- Existing minimal-gate contracts remain green.
- module compile gate remains green (`179/179`, `0 failed`).
