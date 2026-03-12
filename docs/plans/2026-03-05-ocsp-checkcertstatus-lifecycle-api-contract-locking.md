# OCSP CheckCertificateStatus Lifecycle API Contract Locking

## Goal
Expand `CheckCertificateStatus` dependency contract so lifecycle/cleanup OCSP APIs are included, preventing hidden runtime AV risk when module-level symbols degrade.

## Architecture
- RED:
  - Extend `tests/unit/test_ocsp_client_semantics.pas` with a direct helper contract scenario:
    - `RunCheckCertificateStatusDependencyContractScenario`
  - Use deterministic dummy function pointers to build a non-nil baseline.
  - Assert `CheckCertificateStatusDependenciesAvailable=False` when each symbol is nil.
  - New symbols locked:
    - `OCSP_REQUEST_new`
    - `OCSP_REQUEST_free`
    - `OCSP_RESPONSE_free`
    - `OCSP_BASICRESP_free`
    - `OCSP_CERTID_free`
- GREEN:
  - Minimal update in `src/fafafa.ssl.openssl.api.ocsp.pas`:
    - extend `CheckCertificateStatusDependenciesAvailable` to include the 5 lifecycle symbols.
- Keep `TOCSPClient` behavior boundary unchanged:
  - module loaded + missing deps => controlled unsupported
  - module unloaded => `ocspError`

## Files
- Modify: `tests/unit/test_ocsp_client_semantics.pas`
- Modify: `src/fafafa.ssl.openssl.api.ocsp.pas`

## Steps
1. Add dependency-helper contract scenario and run RED.
2. Extend helper dependency set and run GREEN.
3. Run OCSP/CRL regression + compile gate.

## Execution Log (2026-03-05)

### RED
- Modified `tests/unit/test_ocsp_client_semantics.pas`:
  - Added deterministic helper contract scenario for `CheckCertificateStatusDependenciesAvailable`.
  - Added dummy typed stubs to isolate per-symbol missing assertions.
- Command:
  - `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- RED key output:
  - `[FAIL] checkcert-deps request_new missing: request_new missing must report unavailable`
  - `[FAIL] checkcert-deps request_free missing: request_free missing must report unavailable`
  - `[FAIL] checkcert-deps response_free missing: response_free missing must report unavailable`
  - `[FAIL] checkcert-deps basicresp_free missing: basicresp_free missing must report unavailable`
  - `[FAIL] checkcert-deps certid_free missing: certid_free missing must report unavailable`
  - `Results: 60 passed, 5 failed`

### GREEN
- Modified `src/fafafa.ssl.openssl.api.ocsp.pas`:
  - `CheckCertificateStatusDependenciesAvailable` now additionally requires:
    - `OCSP_REQUEST_new`
    - `OCSP_REQUEST_free`
    - `OCSP_RESPONSE_free`
    - `OCSP_BASICRESP_free`
    - `OCSP_CERTID_free`
- Command:
  - `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- GREEN result:
  - `Results: 65 passed, 0 failed`

### Regression
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
  - PASS (`OCSP TEST COMPLETE`)
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
  - PASS (`CRL TEST COMPLETE`)
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
  - PASS (`Results: 26 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
