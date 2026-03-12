# OCSP CheckCertificateStatus Guard Dedup (Single Source)

## Goal
Deduplicate duplicated `CheckCertificateStatus` critical dependency checks so `TOCSPClient.CheckCertificate` and `CheckCertificateStatus` share one source of truth, without behavior drift.

## Architecture
- Add helper in `src/fafafa.ssl.openssl.api.ocsp.pas`:
  - `CheckCertificateStatusDependenciesAvailable: Boolean`
  - covers the same critical symbol set:
    - `OCSP_RESPONSE_status`
    - `OCSP_RESPONSE_get1_basic`
    - `OCSP_BASICRESP_verify`
    - `OCSP_cert_to_id`
    - `OCSP_resp_find_status`
    - `OCSP_check_validity`
- Replace duplicated inline checks:
  - `CheckCertificateStatus` uses helper.
  - `TOCSPClient.CheckCertificate` fail-closed preflight uses helper (when OCSP module loaded and resolver hook is nil).
- No contract change expected; regression-only batch.

## Files
- Modify: `src/fafafa.ssl.openssl.api.ocsp.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`

## Steps
1. Implement helper and switch both call sites.
2. Run OCSP semantics and focused OCSP/CRL regressions.
3. Run compile gate.

## Execution Log (2026-03-05)

### Implementation
- Modified `src/fafafa.ssl.openssl.api.ocsp.pas`:
  - Added shared helper:
    - `CheckCertificateStatusDependenciesAvailable: Boolean`
  - `CheckCertificateStatus` now uses helper as the single dependency source.
- Modified `src/fafafa.ssl.cert.advanced.pas`:
  - `TOCSPClient.CheckCertificate` preflight now uses
    `CheckCertificateStatusDependenciesAvailable` (with existing `osmOCSP` loaded
    scope) instead of duplicated inline symbol list.
- Scope: dedup only, no intended behavior change.

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
