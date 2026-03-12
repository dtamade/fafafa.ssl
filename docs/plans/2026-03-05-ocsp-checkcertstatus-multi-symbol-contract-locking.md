# OCSP CheckCertificateStatus Multi-Symbol Contract Locking

## Goal
Expand OCSP fail-closed contract coverage so `TOCSPClient.CheckCertificate` remains controlled-unsupported when any critical `CheckCertificateStatus` dependency symbol is missing (not just one symbol).

## Architecture
- RED/GREEN (contract locking):
  - Extend `tests/unit/test_ocsp_client_semantics.pas` missing-API scenario to cover each critical symbol individually by temporarily setting it to `nil`.
  - Symbols covered:
    - `OCSP_RESPONSE_status`
    - `OCSP_RESPONSE_get1_basic`
    - `OCSP_BASICRESP_verify`
    - `OCSP_cert_to_id`
    - `OCSP_resp_find_status`
    - `OCSP_check_validity`
  - Assert each case raises controlled unsupported semantic containing `OpenSSL API CheckCertificateStatus`.
- Add module-state contract:
  - Force `UnloadOpenSSLOCSP` with resolver hook disabled.
  - Assert `TOCSPClient.CheckCertificate` does not raise and maps to `ocspError` with stable error semantic.
  - Reload OCSP module in test cleanup to avoid side effects.
- Production source behavior unchanged in this batch; this is contract hardening.

## Files
- Modify: `tests/unit/test_ocsp_client_semantics.pas`

## Steps
1. Update contract tests for multi-symbol missing scenarios.
2. Verify focused semantics test.
3. Run OCSP/CRL regression gate + compile gate.

## Execution Log (2026-03-05)

### Contract Update
- Modified `tests/unit/test_ocsp_client_semantics.pas`:
  - `RunMissingCheckCertificateStatusAPIFailClosedScenario` now validates fail-closed behavior for 6 independent symbol-missing cases.
  - Added local helper `ExpectFailClosedForMissingAPI` to keep assertion semantics uniform.
  - Added `RunOCSPModuleUnloadedSemanticsScenario`:
    - `UnloadOpenSSLOCSP` path is locked to non-raising `ocspError` semantics.
    - test explicitly reloads OCSP module after assertion.

### Verification
- `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
  - PASS (`43 passed, 0 failed`)
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
  - PASS (`OCSP TEST COMPLETE`)
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
  - PASS (`CRL TEST COMPLETE`)
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
  - PASS (`Results: 26 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
