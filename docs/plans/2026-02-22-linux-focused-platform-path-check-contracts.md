# 2026-02-22 Linux-Focused Platform Path-Check Contracts

## Goal

- Strengthen Linux-side verification flow with report output evidence.
- Continue implementing and validating non-Linux platform path-check scripts in dry-run mode (macOS/Android/Windows) without requiring platform runners.

## Architecture / Scope

- Linux:
  - Add `--report-output` support to `run_linux_openssl_matrix_draft.sh`.
  - Ensure report path resolves under project root from `/tmp`.
- Windows:
  - Add `run_windows_winssl_path_check_draft.sh` with dry-run-compatible flow.
- Cross-platform:
  - Add a Linux-focused multi-platform dry-run batch contract for Linux/macOS/Android/Windows.

## Files

- `scripts/run_linux_openssl_matrix_draft.sh`
- `scripts/run_windows_winssl_path_check_draft.sh`
- `tests/scripts/test_linux_openssl_matrix_report_output_contract.sh`
- `tests/scripts/test_windows_winssl_path_check_draft_dryrun_contract.sh`
- `tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_linux_openssl_matrix_report_output_contract.sh`
   - `bash tests/scripts/test_windows_winssl_path_check_draft_dryrun_contract.sh`
   - Expected: fail (linux missing option/windows script missing).
2. GREEN:
   - Implement linux `--report-output`.
   - Implement windows dry-run path-check script.
3. Regression:
   - `bash tests/scripts/test_linux_openssl_matrix_report_output_contract.sh`
   - `bash tests/scripts/test_windows_winssl_path_check_draft_dryrun_contract.sh`
   - `bash tests/scripts/test_android_openssl_path_check_draft_dryrun_contract.sh`
   - `bash tests/scripts/test_linux_macos_matrix_draft_dryrun_contract.sh`
   - `bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`
   - `bash -n scripts/run_linux_openssl_matrix_draft.sh scripts/run_windows_winssl_path_check_draft.sh tests/scripts/test_linux_openssl_matrix_report_output_contract.sh tests/scripts/test_windows_winssl_path_check_draft_dryrun_contract.sh tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh`

## Expected Outputs

- Linux matrix script supports deterministic report output pathing.
- Windows draft script is runnable in Linux dry-run mode.
- Multi-platform dry-run batch contract passes on Linux.
