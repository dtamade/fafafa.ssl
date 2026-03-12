# OCSP Connection Decode Dependency Helper Dedup

## Goal
Deduplicate OCSP response decode dependency checks in `TOpenSSLConnection` to reduce drift risk between:
- `DoIsOCSPResponseVerified`
- `DoGetOCSPResponseStatus`

No behavior change is intended.

## Architecture
- Extract small helpers in `src/fafafa.ssl.openssl.connection.pas`:
  - `OCSPResponseDecodeDependenciesAvailable`
  - `OCSPResponseStatusDependencyAvailable`
- Replace duplicated inline checks with helper calls while preserving existing branch semantics and error strings.
- Run focused OCSP/CRL regression and compile gate.

## Files
- Modify: `src/fafafa.ssl.openssl.connection.pas`

## Steps
1. Add helpers and replace duplicated check expressions.
2. Run OCSP connection regression.
3. Run OCSP/CRL focused regressions + compile gate.
4. Update `task_plan.md`, `findings.md`, `progress.md`.

## Execution Log (2026-03-05)

### Implementation
- Modified: `src/fafafa.ssl.openssl.connection.pas`
  - Added helper: `OCSPResponseDecodeDependenciesAvailable`
  - Added helper: `OCSPResponseStatusDependencyAvailable`
  - Replaced duplicated inline OCSP dependency checks in:
    - `DoIsOCSPResponseVerified`
    - `DoGetOCSPResponseStatus`
- Scope: dedup only, no intended behavior change.

### Regression
- `fpc -Fu./src tests/openssl/test_ocsp_connection_verification_regression.pas -otmp/test_ocsp_connection_verification_regression && ./tmp/test_ocsp_connection_verification_regression`
  - PASS (`Passed: 8, Failed: 0, Skipped: 0`)
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
