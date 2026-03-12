# OCSP Required Stapling ResponseFree Preflight Contract

## Goal
Lock fail-closed preflight semantics in `TOpenSSLConnection.DoIsOCSPResponseVerified`: when `OCSP_RESPONSE_free` is unavailable, connection-side required-stapling validation must stop before DER parsing and return verification failure safely.

## Architecture
- RED:
  - Extend `tests/openssl/test_ocsp_connection_verification_regression.pas` with a contract scenario.
  - Install a counting `d2i_OCSP_RESPONSE` stub and force `OCSP_RESPONSE_free := nil`.
  - Assert required stapling check fails closed and does not call `d2i_OCSP_RESPONSE`.
- GREEN:
  - Minimal hardening in `src/fafafa.ssl.openssl.connection.pas`:
    - add `Assigned(OCSP_RESPONSE_free)` to preflight guard before parsing stapled response.
- Regression:
  - Run focused OCSP connection regression + OCSP/CRL focused tests + compile gate.

## Files
- Modify: `tests/openssl/test_ocsp_connection_verification_regression.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`

## Steps
1. Add RED contract and run focused regression test.
2. Apply minimal preflight hardening in connection path.
3. Run GREEN focused test.
4. Run OCSP/CRL focused regressions and compile gate.
5. Update `task_plan.md`, `findings.md`, `progress.md`.

## Execution Log (2026-03-05)

### RED
- Modified: `tests/openssl/test_ocsp_connection_verification_regression.pas`
  - Added `TestRequiredOCSPStaplingResponseFreeMissingPreflight`.
  - Added counting stub `CountingD2IOCSPResponse` and call counter.
  - Scenario asserts required-stapling fail-closed path must not enter
    `d2i_OCSP_RESPONSE` when `OCSP_RESPONSE_free=nil`.
- Command:
  - `fpc -Fu./src tests/openssl/test_ocsp_connection_verification_regression.pas -otmp/test_ocsp_connection_verification_regression && ./tmp/test_ocsp_connection_verification_regression`
- RED key output:
  - `[FAIL] Expected preflight to block d2i parse when OCSP_RESPONSE_free is missing, got d2i calls=1`
  - `Passed: 4`, `Failed: 1`, `Skipped: 0`

### GREEN
- Modified: `src/fafafa.ssl.openssl.connection.pas`
  - `DoIsOCSPResponseVerified` preflight now additionally requires
    `Assigned(OCSP_RESPONSE_free)` before DER parse.
- Command:
  - `fpc -Fu./src tests/openssl/test_ocsp_connection_verification_regression.pas -otmp/test_ocsp_connection_verification_regression && ./tmp/test_ocsp_connection_verification_regression`
- GREEN output:
  - `Passed: 5`, `Failed: 0`, `Skipped: 0`

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
