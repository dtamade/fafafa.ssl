# OCSP VerifyOCSPResponse BasicRespFree Preflight Contract

## Goal
Lock fail-closed preflight semantics in `VerifyOCSPResponse`: when `OCSP_BASICRESP_free` is unavailable, the helper must exit early and avoid entering response-processing calls.

## Architecture
- RED:
  - Extend `tests/unit/test_ocsp_client_semantics.pas` with a deterministic contract scenario for `VerifyOCSPResponse`.
  - Build a stubbed baseline for `VerifyOCSPResponse`-critical symbols.
  - Set `OCSP_BASICRESP_free := nil` and assert helper preflight prevents `OCSP_RESPONSE_status` execution.
- GREEN:
  - Minimal source hardening in `src/fafafa.ssl.openssl.api.ocsp.pas`:
    - include `OCSP_BASICRESP_free` in `VerifyOCSPResponse` dependency check.
- Regression:
  - Re-run focused OCSP/CRL test set and compile gate.

## Files
- Modify: `tests/unit/test_ocsp_client_semantics.pas`
- Modify: `src/fafafa.ssl.openssl.api.ocsp.pas`

## Steps
1. Add failing contract for missing `OCSP_BASICRESP_free` preflight.
2. Run RED (`test_ocsp_client_semantics`).
3. Apply minimal dependency-guard fix in `VerifyOCSPResponse`.
4. Run GREEN + focused regressions + compile gate.
5. Update `task_plan.md`, `findings.md`, `progress.md` with evidence.

## Execution Log (2026-03-05)

### RED
- Modified: `tests/unit/test_ocsp_client_semantics.pas`
  - Added scenario `RunVerifyOCSPResponseDependencyPreflightScenario`.
  - Added counter-backed stub `CountingOCSPResponseStatus` to detect whether
    preflight guard blocks entry into response-processing path.
- Command:
  - `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- RED key output:
  - `[FAIL] verifyocsp-preflight missing-basicresp_free blocks response_status call: expected response_status calls=0 got=1`
  - `Results: 76 passed, 1 failed`

### GREEN
- Modified: `src/fafafa.ssl.openssl.api.ocsp.pas`
  - `VerifyOCSPResponse` dependency preflight now also requires
    `Assigned(OCSP_BASICRESP_free)`.
- Command:
  - `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- GREEN output:
  - `Results: 77 passed, 0 failed`

### Regression
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
  - PASS (`OCSP TEST COMPLETE`)
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
  - PASS (`CRL TEST COMPLETE`)
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
  - PASS (`Results: 26 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
