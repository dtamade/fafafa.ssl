# OCSP GetStatus ResponseStatus Preflight Contract

## Goal
Lock fail-closed preflight semantics in `TOpenSSLConnection.DoGetOCSPResponseStatus`: when `OCSP_RESPONSE_status` is unavailable, status query must stop before DER parsing and return status-API-unavailable semantic safely.

## Architecture
- Contract locking (no production behavior change expected):
  - Extend `tests/openssl/test_ocsp_connection_verification_regression.pas` with a status-path contract.
  - Install counting `d2i_OCSP_RESPONSE` stub and set `OCSP_RESPONSE_status := nil`.
  - Keep `OCSP_RESPONSE_free` assigned (no-op stub) to isolate only status-API dependency.
  - Assert `GetOCSPResponseStatus` does not invoke parse and returns status-API-unavailable semantic.
- Regression:
  - Run focused OCSP connection regression + OCSP/CRL focused tests + compile gate.

## Files
- Modify: `tests/openssl/test_ocsp_connection_verification_regression.pas`

## Steps
1. Add contract for missing `OCSP_RESPONSE_status` in status query path.
2. Run focused connection regression.
3. Run focused regressions + compile gate.
4. Update `task_plan.md`, `findings.md`, `progress.md`.

## Execution Log (2026-03-05)

### Contract
- Modified: `tests/openssl/test_ocsp_connection_verification_regression.pas`
  - Added `TestOCSPResponseStatusStatusApiMissingPreflight`.
  - Reused counting stub `CountingD2IOCSPResponse`.
  - Reused `NoopOCSPResponseFree` to isolate status-API-only scenario.
- Command:
  - `fpc -Fu./src tests/openssl/test_ocsp_connection_verification_regression.pas -otmp/test_ocsp_connection_verification_regression && ./tmp/test_ocsp_connection_verification_regression`
- Output (key):
  - `=== OCSP status preflight: missing OCSP_RESPONSE_status must block parse ===`
  - `[PASS] Missing OCSP_RESPONSE_status blocks status parse and returns status-API semantic`
  - `Passed: 8`, `Failed: 0`, `Skipped: 0`

### Regression
- `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
  - PASS (`Results: 77 passed, 0 failed`)
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
  - PASS (`OCSP TEST COMPLETE`)
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
  - PASS (`CRL TEST COMPLETE`)
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
  - PASS (`Results: 26 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
