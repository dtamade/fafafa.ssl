# OCSP CheckCertificateStatus API Guard (Fail-Closed)

## Goal
Ensure `TOCSPClient.CheckCertificate` fails closed with a controlled unsupported error when OCSP status-check critical OpenSSL APIs are unavailable and no test resolver hook is active.

## Architecture
- RED:
  - Extend `tests/unit/test_ocsp_client_semantics.pas` with a contract that temporarily nulls a critical OCSP function pointer (`OCSP_RESPONSE_status`) while `OCSPStatusResolverHook=nil`.
  - Assert `TOCSPClient.CheckCertificate` raises controlled unsupported semantic containing:
    - `OpenSSL API CheckCertificateStatus`
- GREEN:
  - Minimal hardening in `src/fafafa.ssl.cert.advanced.pas`:
    - before calling `CheckCertificateStatus` (when resolver hook is nil), guard required OCSP APIs and raise `RaiseUnsupported('OpenSSL API CheckCertificateStatus')` if missing.
  - Keep status mapping and hook behavior unchanged.

## Files
- Modify: `tests/unit/test_ocsp_client_semantics.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`
- Modify: `tests/unit/test_ocsp.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- Expected: new fail-closed contract fails because current behavior returns `ocspError` instead of raising unsupported.

2. GREEN
- same command as RED
- Expected: all semantics pass including new unsupported contract.

3. Regression
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
- `python3 scripts/compile_all_modules.py`

## Execution Log (2026-03-05)

### RED
- Modified `tests/unit/test_ocsp_client_semantics.pas`:
  - Added `RunMissingCheckCertificateStatusAPIFailClosedScenario`.
  - Contract temporarily sets `OCSP_RESPONSE_status=nil` with `OCSPStatusResolverHook=nil`.
- RED key output:
  - `[FAIL] missing-checkcertificatestatus-api must fail closed: expected controlled unsupported exception when OCSP status API is missing`
  - `Results: 23 passed, 1 failed`

### GREEN
- Modified `src/fafafa.ssl.cert.advanced.pas`:
  - Added preflight unsupported guard for missing OCSP status-check critical APIs in `TOCSPClient.CheckCertificate`.
  - Guard scope refined to `TOpenSSLLoader.IsModuleLoaded(osmOCSP)` to avoid behavior drift before OCSP module load.
- Modified `tests/unit/test_ocsp_client_semantics.pas`:
  - Added explicit OCSP module load check in missing-API scenario (`LoadOpenSSLOCSP(GetCryptoLibHandle)`).
- GREEN output:
  - `test_ocsp_client_semantics`: `26 passed, 0 failed`

### Regression
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
  - PASS (`63 passed, 0 failed`)
- `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
  - PASS (`26 passed, 0 failed`)
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
  - PASS (`CRL TEST COMPLETE`)
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
  - PASS (`OCSP TEST COMPLETE`, with fail-closed message on missing API)
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
  - PASS (`Results: 26 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
- Compatibility alignment:
  - Updated `tests/unit/test_ocsp.pas` to accept controlled fail-closed unsupported behavior when OCSP status API is unavailable.
