# 2026-02-22 Android OpenSSL Path Check Dry-Run Contract

## Goal

- Add an Android path-check draft script that can be executed in dry-run mode without Android runner dependencies.

## Architecture / Scope

- Script focuses on command orchestration and environment-prefix composition.
- Contract test validates:
  - dry-run success exit path
  - command execution root (`cd '$PROJECT_ROOT'`)
  - Android env prefix presence
- No real Android compile/runtime required in this batch.

## Files

- `scripts/run_android_openssl_path_check_draft.sh`
- `tests/scripts/test_android_openssl_path_check_draft_dryrun_contract.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_android_openssl_path_check_draft_dryrun_contract.sh`
   - Expected: fail (script missing).
2. GREEN:
   - Implement script with options:
     - `--ndk-root`
     - `--openssl-root`
     - `--abi`
     - `--api-level`
     - `--modules`
     - `--skip-module-tests`
     - `--skip-phase2-dryrun`
     - `--verbose`
     - `--dry-run`
   - Add dry-run fallback roots for NDK/OpenSSL.
3. Regression:
   - `bash tests/scripts/test_android_openssl_path_check_draft_dryrun_contract.sh`
   - `bash tests/scripts/test_linux_macos_matrix_draft_dryrun_contract.sh`
   - `bash -n scripts/run_android_openssl_path_check_draft.sh tests/scripts/test_android_openssl_path_check_draft_dryrun_contract.sh`

## Expected Outputs

- Android dry-run contract passes in current Linux environment.
- Existing Linux/macOS dry-run contracts remain green.
