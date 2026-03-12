# 2026-02-22 Platform Path-Check Failure Contracts + Minimal Gate Integration

## Goal

- Add non-dry-run failure contracts for Windows/Android path-check draft scripts.
- Integrate the Linux-local four-platform dry-run batch contract into the local verification entrypoint `run_minimal_ci_gate.sh`.

## Architecture / Scope

- Failure contracts:
  - `run_windows_winssl_path_check_draft.sh` must fail on non-Windows when not in dry-run mode.
  - `run_android_openssl_path_check_draft.sh` must fail when `--ndk-root` points to a missing directory in non-dry-run mode.
- Local gate integration:
  - `run_minimal_ci_gate.sh` should invoke `tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh` by default.
  - Provide an explicit skip switch for this new step.

## Files

- `scripts/run_minimal_ci_gate.sh`
- `tests/scripts/test_windows_winssl_path_check_draft_failure_contract.sh`
- `tests/scripts/test_android_openssl_path_check_draft_failure_contract.sh`
- `tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_windows_winssl_path_check_draft_failure_contract.sh`
   - `bash tests/scripts/test_android_openssl_path_check_draft_failure_contract.sh`
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - Expected: failure on minimal gate integration (missing platform path-check batch step).
2. GREEN:
   - Update `scripts/run_minimal_ci_gate.sh`:
     - add `--skip-platform-path-checks-dryrun`
     - add default invocation of `tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
3. Regression:
   - `bash tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
   - `bash tests/scripts/test_windows_winssl_path_check_draft_failure_contract.sh`
   - `bash tests/scripts/test_android_openssl_path_check_draft_failure_contract.sh`
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash tests/scripts/test_windows_winssl_path_check_draft_dryrun_contract.sh`
   - `bash tests/scripts/test_android_openssl_path_check_draft_dryrun_contract.sh`
   - `bash -n scripts/run_minimal_ci_gate.sh tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh tests/scripts/test_windows_winssl_path_check_draft_failure_contract.sh tests/scripts/test_android_openssl_path_check_draft_failure_contract.sh`

## Expected Outputs

- Windows/Android non-dry-run failure behavior is contract-locked and reproducible on Linux.
- Minimal local gate includes four-platform dry-run batch by default.
- A skip flag exists for local fast path when needed.
