# Connection Base LastErrorTime and ErrorHistory Safety

## Goal
Fix diagnostic correctness in `TBaseSSLConnection` so `GetHealthStatus.LastErrorTime` reflects the actual last error timestamp (instead of query time), and remove unsafe managed-record shifting in error history overflow handling.

## Architecture
- Add a focused contract test for `TBaseSSLConnection` diagnostics behavior.
- Implement minimal state tracking (`FLastErrorTime`) in the base connection class.
- Replace `Move` on `array of TSSLErrorRecord` (contains managed `string`) with safe element-wise shift.

## Files
- Add: `tests/test_connection_base_last_error_time_contract.pas`
- Modify: `src/fafafa.ssl.connection.base.pas`
- Writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. RED
- Add contract test that asserts:
  - new connection has `LastErrorTime = 0`;
  - after recording one error, `LastErrorTime > 0`;
  - repeated health queries do not refresh `LastErrorTime`.
- Run:
  - `fpc -Fu./src -Fi./src tests/test_connection_base_last_error_time_contract.pas -otmp/test_connection_base_last_error_time_contract && ./tmp/test_connection_base_last_error_time_contract`
- Expected:
  - FAIL before production fix.

2. GREEN
- In `src/fafafa.ssl.connection.base.pas`:
  - add `FLastErrorTime: TDateTime`;
  - initialize it to `0` in constructor;
  - set it in `RecordError`;
  - return it from `GetHealthStatus`.
- Replace overflow branch in `RecordError`:
  - remove `Move(...)` on managed records;
  - shift entries using a `for` loop assignment.

3. Verification
- Focused:
  - `fpc -Fu./src -Fi./src tests/test_connection_base_last_error_time_contract.pas -otmp/test_connection_base_last_error_time_contract && ./tmp/test_connection_base_last_error_time_contract`
  - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
  - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
  - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/openssl/test_openssl_cert_verify_cache_policy.pas -otmp/test_openssl_cert_verify_cache_policy && ./tmp/test_openssl_cert_verify_cache_policy`
- Regression gate:
  - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- New contract test passes after fix.
- Existing OCSP/CRL/cache-policy core contracts remain green.
- Full module compile remains green (`179/179`, `0 failed`).
