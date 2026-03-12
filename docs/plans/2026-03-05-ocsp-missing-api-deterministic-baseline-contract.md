# OCSP Missing-API Deterministic Baseline Contract

## Goal
Eliminate false positives in `RunMissingCheckCertificateStatusAPIFailClosedScenario` by making each missing-symbol subcase run from a deterministic all-symbol baseline.

## Architecture
- RED:
  - Add explicit baseline assertion in missing-API scenario:
    - `CheckCertificateStatusDependenciesAvailable` must be true before any subcase.
  - Expect failure in environments where some symbols are already unavailable.
- GREEN:
  - Install deterministic dummy stubs for all CheckCertificateStatus dependencies inside the scenario.
  - For each subcase, restore to deterministic baseline (not runtime original pointers), then nil one symbol.
  - Keep final cleanup restoring original pointers.
- No production behavior change; test contract hardening only.

## Files
- Modify: `tests/unit/test_ocsp_client_semantics.pas`

## Steps
1. Add baseline assertion and run RED.
2. Install deterministic baseline + isolate per-subcase restore, run GREEN.
3. Run focused OCSP/CRL regressions and compile gate.

## Execution Log (2026-03-05)

### RED
- Modified `tests/unit/test_ocsp_client_semantics.pas`:
  - Added assertion:
    - `missing-checkcertificatestatus-api deterministic baseline available`
- Command:
  - `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- RED key output:
  - `[FAIL] missing-checkcertificatestatus-api deterministic baseline available: expected all CheckCertificateStatus dependencies assigned before missing-symbol subcases`
  - `Results: 65 passed, 1 failed`

### GREEN
- Modified `tests/unit/test_ocsp_client_semantics.pas`:
  - Added local helper `InstallDeterministicDependencyBaseline` in missing-API scenario.
  - Baseline now sets typed dummy stubs for all dependencies:
    - `OCSP_RESPONSE_status`
    - `OCSP_RESPONSE_get1_basic`
    - `OCSP_BASICRESP_verify`
    - `OCSP_cert_to_id`
    - `OCSP_resp_find_status`
    - `OCSP_check_validity`
    - `OCSP_REQUEST_new`
    - `OCSP_REQUEST_free`
    - `OCSP_RESPONSE_free`
    - `OCSP_BASICRESP_free`
    - `OCSP_CERTID_free`
  - Each subcase now restores to deterministic baseline between nil injections.
  - Final cleanup keeps restoring runtime original pointers.
- Command:
  - `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- GREEN output:
  - `Results: 66 passed, 0 failed`

### Regression
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
  - PASS (`OCSP TEST COMPLETE`)
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
  - PASS (`CRL TEST COMPLETE`)
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
  - PASS (`Results: 26 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
