# OCSP Cache Small MaxEntries Shard Limit Ceil Fix

## Goal
Ensure very small positive `MaxEntries` values (e.g. `1`) still allow storing entries. Current floor-division shard limit can become zero and evict every inserted entry.

## Architecture
- RED contract in `tests/ocsp/test_ocsp_cache.pas`:
  - create cache with `MaxEntries=1`
  - put one entry
  - entry must be retrievable
- GREEN minimal fix in `src/fafafa.ssl.ocsp.cache.pas`:
  - compute shard limit with ceil-division when `FMaxEntries>0`
  - keep `FMaxEntries<=0` behavior unchanged (`0`)

## Files
- Modify: `tests/ocsp/test_ocsp_cache.pas`
- Modify: `src/fafafa.ssl.ocsp.cache.pas`

## Steps
1. RED
- `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && timeout 45s ./tmp/test_ocsp_cache`
- Expected: small-maxentries contract fails.

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
  - Added `TestSmallMaxEntriesStillStoresEntry`.

- Command:
  - `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && timeout 45s ./tmp/test_ocsp_cache`
- RED result (key):
  - `[8] Small MaxEntries should still store at least one entry... FAIL: Entry should be retrievable when MaxEntries=1`

### GREEN
- Modified: `src/fafafa.ssl.ocsp.cache.pas`
  - In `Put` and `EnforceSizeLimitInShard`, changed per-shard limit from floor division:
    - `FMaxEntries div SHARD_COUNT`
  - to ceil division for positive limits:
    - `(FMaxEntries + SHARD_COUNT - 1) div SHARD_COUNT`

- Command:
  - `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && timeout 45s ./tmp/test_ocsp_cache`
- GREEN result:
  - `Total Tests: 30`
  - `Passed: 30`
  - `Failed: 0`

### Regression
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
  - PASS (`OCSP TEST COMPLETE`)
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
  - PASS (`Results: 26 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
