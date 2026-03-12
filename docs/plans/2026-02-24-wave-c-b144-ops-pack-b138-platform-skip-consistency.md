# 2026-02-24 Wave C B144 Ops Pack B138 Platform Skip Consistency

## Goal

- Add end-to-end consistency contract for `run_wave_c_local_guard_ops_pack.sh` to ensure B138 platform skip semantics are controllable and observable.

## Architecture / Scope

- Script:
  - `scripts/run_wave_c_local_guard_ops_pack.sh`
  - Add option:
    - `--skip-platform-path-checks-dryrun`
  - Forward option to:
    - `scripts/run_wave_c_pre_ci_reenable_full_gate.sh`
  - Add observability in ops report:
    - `b138_platform_path_checks_mode` (`ENABLED`/`SKIPPED`)
- Contract:
  - `tests/scripts/test_wave_c_local_guard_ops_pack_b138_platform_skip_passthrough.sh`
  - Validate:
    - default mode generates B125 platform path-check log via B138/B129/B125 chain.
    - skip mode suppresses that log and keeps `SKIP/SKIPPED` evidence in B125/B129 reports.

## Files

- `scripts/run_wave_c_local_guard_ops_pack.sh`
- `tests/scripts/test_wave_c_local_guard_ops_pack_b138_platform_skip_passthrough.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_b138_platform_skip_passthrough.sh`
   - Expected: fail (missing ops-level observability/passthrough support).
2. GREEN:
   - Implement skip passthrough and options section in ops pack report.
3. Regression:
   - `bash tests/scripts/test_wave_c_local_guard_ops_pack_b138_platform_skip_passthrough.sh`
   - `bash tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_platform_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_guard_oncall_platform_path_checks_passthrough.sh`
   - `bash tests/scripts/test_wave_c_local_first_guard_bundle_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash -n scripts/run_wave_c_local_guard_ops_pack.sh tests/scripts/test_wave_c_local_guard_ops_pack_b138_platform_skip_passthrough.sh`

## Expected Outputs

- Ops pack can control B138 platform path-check execution via one switch.
- Ops pack report explicitly records the mode (`ENABLED`/`SKIPPED`) for auditability.
