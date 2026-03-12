# OCSP d2i Missing Preflight Contracts (Status + Required Stapling)

## Goal
Lock fail-closed preflight semantics for `d2i_OCSP_RESPONSE` dependency in connection-side OCSP paths:
- status query (`DoGetOCSPResponseStatus`) must return API-unavailable semantic before downstream status resolver path;
- required stapling (`ValidateRequiredOCSPStapling -> DoIsOCSPResponseVerified`) must fail closed with `X509_V_ERR_OCSP_VERIFY_FAILED` before downstream status resolver path.

## Architecture
- Contract locking (no production behavior change expected):
  - Extend `tests/openssl/test_ocsp_connection_verification_regression.pas` with two scenarios:
    - `TestOCSPResponseStatusD2IMissingPreflight`
    - `TestRequiredOCSPStaplingD2IMissingPreflight`
  - Add counter stub `CountingOCSPResponseStatus` to detect unintended downstream status resolver calls.
  - Force `d2i_OCSP_RESPONSE := nil` while keeping `OCSP_RESPONSE_free` and `OCSP_RESPONSE_status` assigned to isolate `d2i` dependency only.
  - Assert:
    - status path returns `OCSP API not available` and `status calls = 0`;
    - required-stapling path returns `False`, sets `verify_result = X509_V_ERR_OCSP_VERIFY_FAILED`, and `status calls = 0`.
- Regression:
  - focused OCSP connection regression
  - compile gate (`compile_all_modules.py`)

## Files
- Modify: `tests/openssl/test_ocsp_connection_verification_regression.pas`

## Steps
1. Add two d2i-missing preflight contract scenarios and register them.
2. Run focused connection regression.
3. Run compile gate.
4. Update `task_plan.md`, `findings.md`, `progress.md`.

## Execution Log (2026-03-05)

### Contract
- Modified: `tests/openssl/test_ocsp_connection_verification_regression.pas`
  - Added counter:
    - `GOCSPResponseStatusCalls`
    - `CountingOCSPResponseStatus`
  - Added:
    - `TestRequiredOCSPStaplingD2IMissingPreflight`
    - `TestOCSPResponseStatusD2IMissingPreflight`
  - Registered both in main test entry.
- Command:
  - `fpc -Fu./src tests/openssl/test_ocsp_connection_verification_regression.pas -otmp/test_ocsp_connection_verification_regression && ./tmp/test_ocsp_connection_verification_regression`
- Output (key):
  - `[PASS] Missing d2i_OCSP_RESPONSE blocks required-stapling path and fails closed`
  - `[PASS] Missing d2i_OCSP_RESPONSE blocks status path and returns API-unavailable semantic`
  - `Passed: 12`
  - `Failed: 0`
  - `Skipped: 0`

### Regression
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)

### Notes
- No production source change was required in this batch.
