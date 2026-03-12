# OCSP Cache NextUpdate=0 Fail-Closed Semantics

## Goal
Avoid treating cache entries with unknown/zero `NextUpdate` as non-expiring. Unknown validity in OCSP cache should fail closed to prevent stale response reuse.

## Architecture
- Add RED contract in `tests/ocsp/test_ocsp_cache.pas`:
  - `TOCSPCacheEntry` with `NextUpdate=0` must be expired.
- Minimal source fix in `src/fafafa.ssl.ocsp.cache.pas`:
  - `TOCSPCacheEntry.IsExpired` returns `True` when `NextUpdate<=0`.
- Keep existing behavior where `Put(..., ANextUpdate=0)` applies default TTL.

## Files
- Modify: `tests/ocsp/test_ocsp_cache.pas`
- Modify: `src/fafafa.ssl.ocsp.cache.pas`

## Steps
1. RED
- `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && ./tmp/test_ocsp_cache`
- Expected: new `NextUpdate=0` assertion fails.

2. GREEN
- Same command as RED.
- Expected: all tests pass.

3. Regression
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
- `python3 scripts/compile_all_modules.py`

## Execution Log (2026-03-04)

### RED
- Modified: `tests/ocsp/test_ocsp_cache.pas`
  - Extended `TestCacheEntryIsExpired` with contract:
    - `Entry.NextUpdate := 0`
    - `Entry.IsExpired` must be `True` (fail-closed)

- Command:
  - `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && ./tmp/test_ocsp_cache`
- RED result (key):
  - `[23] TOCSPCacheEntry.IsExpired... FAIL: Entry with NextUpdate=0 should be treated as expired`

### GREEN
- Modified: `src/fafafa.ssl.ocsp.cache.pas`
  - `TOCSPCacheEntry.IsExpired` changed from:
    - `NextUpdate=0 => not expired`
  - to:
    - `NextUpdate<=0 => expired` (fail-closed)

- Command:
  - `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && ./tmp/test_ocsp_cache`
- GREEN result:
  - `Total Tests: 26`
  - `Passed: 26`
  - `Failed: 0`

### Regression
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
  - PASS (`OCSP TEST COMPLETE`)
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
  - PASS (`Results: 26 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
