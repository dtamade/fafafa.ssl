# OCSP/CRL Nil-Handle Fail-Closed Alignment

## Goal
Align fail-closed argument/handle semantics across OCSP and CRL advanced APIs:
- `TOCSPClient.CheckCertificate` must reject nil certificate handles before resolver/network path.
- `TCRLManagerImpl.GetRevokedDate/GetRevocationReason` nil-certificate behavior is explicitly contract-pinned.

## Architecture
- RED:
  - Extend `tests/unit/test_ocsp_client_semantics.pas` with nil-handle contract using a strict resolver hook.
  - Extend `tests/unit/test_crl_revocation_semantics.pas` with explicit nil contracts for:
    - `GetRevokedDate(nil)`
    - `GetRevocationReason(nil)`
- GREEN:
  - Minimal source change in `src/fafafa.ssl.cert.advanced.pas`:
    - add fail-closed guards in `TOCSPClient.CheckCertificate` for:
      - `ACert=nil`
      - `AIssuer=nil`
      - `LCertEx.X509Handle=nil`
      - `LIssuerEx.X509Handle=nil`
    - keep existing status-mapping logic unchanged.

## Files
- Modify: `tests/unit/test_ocsp_client_semantics.pas`
- Modify: `tests/unit/test_crl_revocation_semantics.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- Expected: nil-handle scenario fails because current OCSP path does not raise preflight certificate-handle error.

2. GREEN
- same command as RED
- Expected: nil-handle scenario returns controlled certificate-handle exception.

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
  - Added `TNilX509Certificate` test double (`GetX509Handle=nil`).
  - Added strict resolver hook (`StrictHandleOCSPStatus`) and contract:
    - `nil-handle ocsp must fail closed`.
- RED key output:
  - `[FAIL] nil-handle ocsp must fail closed: expected controlled exception for nil X509 handle`
  - `Results: 14 passed, 1 failed`

### GREEN
- Modified `src/fafafa.ssl.cert.advanced.pas`:
  - `TOCSPClient.CheckCertificate` now guards nil cert/issuer and nil X509 handles before resolver path.
- GREEN result:
  - `test_ocsp_client_semantics`: `16 passed, 0 failed`

### CRL Contract Pinning
- Modified `tests/unit/test_crl_revocation_semantics.pas`:
  - Added contracts:
    - `nil cert revoked-date returns controlled certificate-access error`
    - `nil cert revocation-reason returns controlled certificate-access error`
- Result:
  - `test_crl_revocation_semantics`: `57 passed, 0 failed`

### Regression
- `test_unit_crl`: PASS (`CRL TEST COMPLETE`)
- `test_unit_ocsp`: PASS (`OCSP TEST COMPLETE`)
- `test_ocsp_crl_interface`: PASS (`26 passed, 0 failed`)
- `compile_all_modules.py`: PASS (`179/179`, `0 failed`, `100.0%`)

## Follow-up Contract Locking (2026-03-05)

### Added Contracts
- In `tests/unit/test_ocsp_client_semantics.pas`:
  - `nil-issuer ocsp returns controlled issuer-access error`
  - `nil-issuer-handle ocsp returns controlled issuer-access error`
- In `tests/unit/test_crl_revocation_semantics.pas`:
  - `isRevoked no-crl returns invalid-data semantic`
  - `getRevokedDate no-crl returns invalid-data semantic`
  - `getRevocationReason no-crl returns invalid-data semantic`

### Verification
- `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
  - PASS (`22 passed, 0 failed`)
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
  - PASS (`63 passed, 0 failed`)
