# OCSP Cache Contains Expired Entry Fail-Closed Semantics

## Goal
Ensure `TOCSPResponseCache.Contains` does not report expired entries as present. Expired entries should be treated as unavailable and cleaned up on `Contains` check.

## Architecture
- Add RED contract in `tests/ocsp/test_ocsp_cache.pas`:
  - put an already expired entry
  - `Contains` must return `False`
  - entry should be lazily removed (`GetCount=0`)
- Minimal source fix in `src/fafafa.ssl.ocsp.cache.pas`:
  - `Contains` validates entry via `IsValid`
  - removes expired entry before returning

## Files
- Modify: `tests/ocsp/test_ocsp_cache.pas`
- Modify: `src/fafafa.ssl.ocsp.cache.pas`

## Steps
1. RED
- `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && ./tmp/test_ocsp_cache`
- Expected: new Contains-expired contract fails.

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
  - Added `TestContainsExpiredEntry`:
    - put expired entry
    - `Contains` must return false
    - `GetCount` must drop to 0

- Command:
  - `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && ./tmp/test_ocsp_cache`
- RED result (key):
  - `[9] Contains should reject expired entry and cleanup... FAIL: Contains should return false for expired entry`

### GREEN
- Modified: `src/fafafa.ssl.ocsp.cache.pas`
  - `TOCSPResponseCache.Contains` changed to:
    - read entry
    - return `True` only if `Entry.IsValid`
    - delete expired entry and return `False`

- Command:
  - `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && ./tmp/test_ocsp_cache`
- GREEN result:
  - `Total Tests: 27`
  - `Passed: 27`
  - `Failed: 0`

### Regression
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
  - PASS (`OCSP TEST COMPLETE`)
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
  - PASS (`Results: 26 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
