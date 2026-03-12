# Connection Base RecordError Overflow Retention Contract

## Goal
Lock `TBaseSSLConnection.RecordError` overflow behavior so diagnostics always retain the latest `FMaxErrorHistory` entries in stable order.

## Architecture
- Extend the existing connection-base diagnostics contract test.
- Add assertions for overflow capacity and ordering via `GetDiagnosticInfo.ErrorHistory`.
- This is a contract-hardening batch (no intended production behavior change).

## Files
- Modify: `tests/test_connection_base_last_error_time_contract.pas`
- Writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Add overflow retention assertions
- Inject 125 synthetic errors through `RecordError`.
- Assert:
  - `Length(ErrorHistory) = 100` (default max history)
  - first retained message is `overflow-026`
  - last retained message is `overflow-125`

2. Verification
- Focused:
  - `fpc -Fu./src -Fi./src tests/test_connection_base_last_error_time_contract.pas -otmp/test_connection_base_last_error_time_contract && ./tmp/test_connection_base_last_error_time_contract`
  - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
  - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Regression gate:
  - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Overflow retention contract passes.
- Existing OCSP/CRL contracts remain green.
- Full module compile remains green.
