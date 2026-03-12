# Connection Base GetError LastErrorTime Alignment

## Goal
Align diagnostics behavior in `TBaseSSLConnection`: when `GetError` reports a non-`sslErrNone` error, `GetHealthStatus` must expose a populated `LastErrorTime` and consistent `LastError`.

## Architecture
- Extend existing connection-base diagnostics contract test to cover `GetError` path.
- Apply minimal implementation change in `GetError` to synchronize timestamp/message state for non-none errors.
- Keep behavior fail-safe and backward compatible for `sslErrNone`.

## Files
- Modify: `tests/test_connection_base_last_error_time_contract.pas`
- Modify: `src/fafafa.ssl.connection.base.pas`
- Writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. RED
- Extend test with a `GetError` scenario:
  - force backend error code to `sslErrIO`;
  - call `GetError(-1)`;
  - assert `GetHealthStatus.LastError = sslErrIO`;
  - assert `GetHealthStatus.LastErrorTime > 0`.
- Run:
  - `fpc -Fu./src -Fi./src tests/test_connection_base_last_error_time_contract.pas -otmp/test_connection_base_last_error_time_contract && ./tmp/test_connection_base_last_error_time_contract`
- Expected:
  - FAIL before production fix (`LastErrorTime` remains `0` on `GetError` path).

2. GREEN
- In `src/fafafa.ssl.connection.base.pas`, update `GetError`:
  - keep `Result := DoGetError(ARet)`;
  - if `Result <> sslErrNone`, sync:
    - `FLastErrorCode := Result`
    - `FLastErrorString := SSLErrorToString(Result)`
    - `FLastErrorTime := Now`

3. Verification
- Focused:
  - `fpc -Fu./src -Fi./src tests/test_connection_base_last_error_time_contract.pas -otmp/test_connection_base_last_error_time_contract && ./tmp/test_connection_base_last_error_time_contract`
  - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
  - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Regression gate:
  - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Extended diagnostics contract passes.
- No regression in OCSP/CRL core contracts.
- Full module compile remains green.
