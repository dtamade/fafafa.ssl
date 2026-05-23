# Task Plan: Compile all modules isolated unit output

## Goal
Keep the default batch compile gate stable by isolating per-unit FPC output directories and eliminating the shared-`-FU` compiler AV.

## Status
Complete

## Current Plan
- [docs/plans/2026-05-23-compile-all-modules-isolated-unit-output.md](docs/plans/2026-05-23-compile-all-modules-isolated-unit-output.md)

## Done
- Reproduced the batch compile failure boundary where `fafafa.ssl.pkcs11.engine.pas` triggered an internal exception only under the shared-output batch script.
- Moved `scripts/compile_all_modules.py` to per-unit `-FU` subdirectories.
- Added a contract test that locks the per-unit output isolation behavior.

## Verification
- `bash tests/scripts/test_compile_all_modules_unit_output_isolation_contract.sh`
- `python3 -m py_compile scripts/compile_all_modules.py`
- `python3 -u scripts/compile_all_modules.py`
- `git diff --check`
