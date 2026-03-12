# OCSP Cache Empty-Serial Fail-Closed Guard

## Goal
Prevent OCSP cache key pollution from empty serial numbers. Empty serial inputs must not create cache entries and must return miss semantics.

## Architecture
- RED: add contract in `tests/ocsp/test_ocsp_cache.pas` for empty serial behavior.
- GREEN: minimal hardening in `src/fafafa.ssl.ocsp.cache.pas`:
  - `Put`: ignore empty serial
  - `Get`: empty serial returns miss (and updates miss stats)
  - `Contains`: empty serial returns false
  - `Remove`: no-op for empty serial

## Files
- Modify: `tests/ocsp/test_ocsp_cache.pas`
- Modify: `src/fafafa.ssl.ocsp.cache.pas`

## Steps
1. RED
- `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && timeout 45s ./tmp/test_ocsp_cache`
- Expected: empty-serial contract fails.

2. GREEN
- Same command as RED.
- Expected: all cache tests pass.

3. Regression
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
- `python3 scripts/compile_all_modules.py`

## Execution Log (2026-03-04)

### RED
- Modified: `tests/ocsp/test_ocsp_cache.pas`
  - Added `TestEmptySerialNumberRejected`:
    - `Put(emptySerial, response)` must not increase count
    - `Contains(emptySerial)=False`
    - `Get(emptySerial)=False`

- Command:
  - `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && timeout 45s ./tmp/test_ocsp_cache`
- RED result (key):
  - `[6] Empty serial number should not be cached... FAIL: Empty serial should not create cache entry`

### GREEN
- Modified: `src/fafafa.ssl.ocsp.cache.pas`
  - `Get`: fail-closed miss for empty serial + miss stats update.
  - `Put`: early-exit for empty serial.
  - `Contains`: early false for empty serial.
  - `Remove`: early-exit for empty serial.

- Command:
  - `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && timeout 45s ./tmp/test_ocsp_cache`
- GREEN result:
  - `Total Tests: 29`
  - `Passed: 29`
  - `Failed: 0`

### Regression
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
  - PASS (`OCSP TEST COMPLETE`)
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
  - PASS (`Results: 26 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
