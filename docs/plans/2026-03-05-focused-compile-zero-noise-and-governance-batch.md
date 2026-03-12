# Focused Compile Zero-Noise Contract and Governance Batch

## Goal
Lock in the newly achieved focused compile baseline (`0 warning / 0 note`) and provide a single batch entrypoint for warning-noise governance contracts.

## Architecture
- Add a focused compile contract script that:
  - compiles `tests/openssl/test_openssl_chain_issuer_selection.pas`
  - runs the produced binary
  - fails on any warning or note in compiler output
  - verifies key PASS marker in runtime output
- Add a small governance batch script that runs:
  - focused zero-noise contract
  - deprecated warning-scope whitelist contract

## Scope
- Add: `tests/scripts/test_focused_compile_zero_noise_contract.sh`
- Add: `tests/scripts/test_warning_noise_governance_contract_batch.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Implement focused zero-noise contract script.
2. Implement governance batch runner script.
3. Verify script syntax and execution:
   - `bash -n tests/scripts/test_focused_compile_zero_noise_contract.sh`
   - `bash tests/scripts/test_focused_compile_zero_noise_contract.sh`
   - `bash -n tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
4. Run module compile gate:
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Focused contract PASS with no warning/note patterns in compile output.
- Governance batch PASS.
- Full module compile remains green.
