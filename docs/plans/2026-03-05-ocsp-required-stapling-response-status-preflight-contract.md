# OCSP Required Stapling ResponseStatus Preflight Contract

## Goal
Lock fail-closed preflight semantics in `TOpenSSLConnection.DoIsOCSPResponseVerified`: when `OCSP_RESPONSE_status` is unavailable, required-stapling validation must stop before DER parsing and return verification failure safely.

## Architecture
- RED:
  - Extend `tests/openssl/test_ocsp_connection_verification_regression.pas` with a required-stapling contract scenario.
  - Install counting `d2i_OCSP_RESPONSE` stub and set `OCSP_RESPONSE_status := nil`.
  - Assert required-stapling validation fails closed and does not invoke DER parse.
- GREEN:
  - Minimal hardening in `src/fafafa.ssl.openssl.connection.pas`:
    - include `Assigned(OCSP_RESPONSE_status)` in `DoIsOCSPResponseVerified` preflight before parse.
- Regression:
  - Run focused OCSP connection regression + OCSP/CRL focused tests + compile gate.

## Files
- Modify: `tests/openssl/test_ocsp_connection_verification_regression.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`

## Steps
1. Add RED contract for missing `OCSP_RESPONSE_status` in required-stapling verification path.
2. Run focused connection regression (expect RED).
3. Apply minimal preflight hardening in `DoIsOCSPResponseVerified`.
4. Run GREEN + focused regressions + compile gate.
5. Update `task_plan.md`, `findings.md`, `progress.md`.

## Execution Log (2026-03-05)

### RED
- Modified: `tests/openssl/test_ocsp_connection_verification_regression.pas`
  - Added `NoopOCSPResponseFree`.
  - Added scenario: `TestRequiredOCSPStaplingResponseStatusMissingPreflight`.
  - Scenario locks required-stapling path to fail closed before parse when
    `OCSP_RESPONSE_status=nil`.
- Command:
  - `fpc -Fu./src tests/openssl/test_ocsp_connection_verification_regression.pas -otmp/test_ocsp_connection_verification_regression && ./tmp/test_ocsp_connection_verification_regression`
- RED key output:
  - `[FAIL] Expected preflight to block d2i parse when OCSP_RESPONSE_status is missing, got d2i calls=1`
  - `Passed: 6`, `Failed: 1`, `Skipped: 0`

### GREEN
- Modified: `src/fafafa.ssl.openssl.connection.pas`
  - `DoIsOCSPResponseVerified` preflight now additionally requires
    `Assigned(OCSP_RESPONSE_status)` before DER parse.
- Command:
  - `fpc -Fu./src tests/openssl/test_ocsp_connection_verification_regression.pas -otmp/test_ocsp_connection_verification_regression && ./tmp/test_ocsp_connection_verification_regression`
- GREEN output:
  - `Passed: 7`, `Failed: 0`, `Skipped: 0`

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
