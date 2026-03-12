# OCSP Required Stapling Module-Load-Failure Preflight Contract

## Goal
Lock fail-closed boundary semantics in `TOpenSSLConnection.ValidateRequiredOCSPStapling` / `DoIsOCSPResponseVerified`:
- when OCSP module loading fails at runtime, required-stapling validation must stop before DER parse;
- connection verification result must remain fail-closed (`X509_V_ERR_OCSP_VERIFY_FAILED`).

## Architecture
- Contract locking (no production behavior change expected):
  - Extend `tests/openssl/test_ocsp_connection_verification_regression.pas` with a required-stapling scenario.
  - Reuse test seam override `TOpenSSLConnectionAccess.EnsureOCSPModuleLoaded` with `GForceOCSPModuleLoadFailure := True`.
  - Install `CountingD2IOCSPResponse` and keep decode/status pointers assigned to isolate only module-load failure.
  - Assert:
    - `CheckRequiredOCSPStapling(True)` returns `False`
    - verify result is `X509_V_ERR_OCSP_VERIFY_FAILED`
    - `d2i_OCSP_RESPONSE` call count stays `0`.
- Regression:
  - focused OCSP connection regression
  - compile gate (`compile_all_modules.py`)

## Files
- Modify: `tests/openssl/test_ocsp_connection_verification_regression.pas`

## Steps
1. Add required-stapling module-load-failure contract scenario and register it.
2. Run focused connection regression.
3. Run compile gate.
4. Update `task_plan.md`, `findings.md`, `progress.md`.

## Execution Log (2026-03-05)

### Contract
- Modified: `tests/openssl/test_ocsp_connection_verification_regression.pas`
  - Added `TestRequiredOCSPStaplingModuleLoadFailurePreflight`.
  - Reused existing seam:
    - `TOpenSSLConnectionAccess.EnsureOCSPModuleLoaded`
    - `GForceOCSPModuleLoadFailure`
  - Reused `CountingD2IOCSPResponse`, `NoopOCSPResponseFree`, `DummyOCSPResponseStatus`.
- Command:
  - `fpc -Fu./src tests/openssl/test_ocsp_connection_verification_regression.pas -otmp/test_ocsp_connection_verification_regression && ./tmp/test_ocsp_connection_verification_regression`
- Output (key):
  - `=== Required OCSP stapling preflight: module load failure must block parse ===`
  - `[PASS] Module load failure blocks stapled response parse and required stapling fails closed`
  - `Passed: 10`
  - `Failed: 0`
  - `Skipped: 0`

### Notes
- No production source change was required in this batch.
- Existing implementation already satisfied the boundary contract.
