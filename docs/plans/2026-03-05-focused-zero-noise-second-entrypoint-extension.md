# Focused Zero-Noise Second Entrypoint Extension

## Goal
Extend focused zero-noise governance from a single representative test to two representative OpenSSL entrypoints, reducing single-test bias.

## Architecture
- Keep existing focused contract unchanged:
  - `tests/scripts/test_focused_compile_zero_noise_contract.sh`
- Add a second focused contract:
  - compile and run `tests/openssl/test_ocsp_connection_verification_regression.pas`
  - fail on any `Warning:` / `Note:` / issued-count summary noise
  - validate runtime PASS marker
- Wire the new contract into:
  - `tests/scripts/test_warning_noise_governance_contract_batch.sh`

## Scope
- Add: `tests/scripts/test_focused_compile_zero_noise_ocsp_regression_contract.sh`
- Modify: `tests/scripts/test_warning_noise_governance_contract_batch.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED:
   - update governance batch list to include new contract script path (before script exists)
   - run:
     - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - expected: fail (`missing contract script`)
2. GREEN:
   - implement second focused zero-noise contract script
3. Regression:
   - `bash -n tests/scripts/test_focused_compile_zero_noise_ocsp_regression_contract.sh`
   - `bash -n tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `bash tests/scripts/test_focused_compile_zero_noise_ocsp_regression_contract.sh`
   - `bash tests/scripts/test_warning_noise_governance_contract_batch.sh`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Both focused contracts pass with zero warning/note compile noise.
- Governance batch now covers:
  - chain issuer selection focused contract
  - OCSP verification regression focused contract
  - deprecated scope whitelist contract
- Full module compile remains green (`179/179`, `0 failed`).
