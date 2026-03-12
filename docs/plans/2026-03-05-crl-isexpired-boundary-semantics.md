# CRL IsExpired Boundary Semantics (now == nextUpdate)

## Goal
Pin `ICRLManager.IsExpired` boundary behavior at `now == nextUpdate`.
Adopt fail-closed semantics so CRL is considered expired at the boundary (`>=`), preventing `>`/`>=` drift.

## Architecture
- RED: extend `tests/unit/test_crl_revocation_semantics.pas` with deterministic boundary contract:
  - load fixture CRL and capture `GetNextUpdate`
  - inject deterministic current-time seam in CRL manager unit
  - assert:
    - `IsExpired=True` when `now == nextUpdate`
    - `IsExpired=False` when `now < nextUpdate`
- GREEN: minimal source fix in `src/fafafa.ssl.cert.advanced.pas`:
  - add `CRLNowProviderHook` test seam (default nil)
  - update `TCRLManagerImpl.IsExpired` to use seam when provided
  - change comparison from `>` to `>=`

## Files
- Modify: `tests/unit/test_crl_revocation_semantics.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Expected: boundary assertion (`now == nextUpdate`) fails under current `>` behavior.

2. GREEN
- same command as RED.
- Expected: all assertions pass.

3. Regression
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
- `python3 scripts/compile_all_modules.py`

## Execution Log (2026-03-05)

### RED
- Modified `tests/unit/test_crl_revocation_semantics.pas`:
  - Added deterministic boundary contracts using `CRLNowProviderHook` seam:
    - `isExpired true when now equals nextUpdate`
    - `isExpired false when now is before nextUpdate`
- RED key output:
  - `[FAIL] isExpired true when now equals nextUpdate: expected expired at boundary now==nextUpdate`
  - `Results: 38 passed, 1 failed`

### GREEN
- Modified `src/fafafa.ssl.cert.advanced.pas`:
  - Added `TCRLNowProvider` + `CRLNowProviderHook` (nil in production).
  - `IsExpired` now uses seam time when provided.
  - Boundary comparison changed from `>` to `>=`.
- GREEN result:
  - `Results: 39 passed, 0 failed` (`test_crl_revocation_semantics`)

### Regression
- `test_unit_crl`: PASS (`CRL TEST COMPLETE`)
- `test_unit_ocsp`: PASS (`OCSP TEST COMPLETE`)
- `test_ocsp_crl_interface`: PASS (`26 passed, 0 failed`)
- `compile_all_modules.py`: PASS (`179/179`, `0 failed`, `100.0%`)
