# OCSP Cache Lock-Order Deadlock Guard (Get/Put vs Stats)

## Goal
Eliminate lock-order inversion between shard locks and stats lock in `TOCSPResponseCache`, and add a deterministic stress contract to guard against regression deadlocks.

## Architecture
- RED contract in `tests/ocsp/test_ocsp_cache.pas`:
  - run concurrent `Get/Put` with concurrent `GetStats/ResetStats`
  - detect stalled concurrent progress as potential deadlock
- Minimal source fix in `src/fafafa.ssl.ocsp.cache.pas`:
  - enforce lock order `shards -> stats`
  - avoid calling `GetCount` (shard locks) while holding `FStatsLock`

## Files
- Modify: `tests/ocsp/test_ocsp_cache.pas`
- Modify: `src/fafafa.ssl.ocsp.cache.pas`

## Steps
1. RED
- `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && timeout 40s ./tmp/test_ocsp_cache`
- Expected: deadlock/stall contract fails.

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
  - Added concurrent stress contract:
    - `TGetPutStressThread`
    - `TStatsStressThread`
    - `TestConcurrentStatsAndGetNoDeadlock`

- Command:
  - `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && timeout 40s ./tmp/test_ocsp_cache`
- RED result (key):
  - `[19] Concurrent Get/Put and GetStats/ResetStats should not deadlock... FAIL: Potential deadlock detected ...`

### GREEN
- Modified: `src/fafafa.ssl.ocsp.cache.pas`
  - `GetStats`:
    - call `GetCount` before entering `FStatsLock`
  - `ResetStats`:
    - call `GetCount` before entering `FStatsLock`
  - Lock order unified to avoid `stats -> shards` path.

- Command:
  - `fpc -Fu./src tests/ocsp/test_ocsp_cache.pas -otmp/test_ocsp_cache && timeout 45s ./tmp/test_ocsp_cache`
- GREEN result:
  - `Total Tests: 28`
  - `Passed: 28`
  - `Failed: 0`

### Regression
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
  - PASS (`OCSP TEST COMPLETE`)
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
  - PASS (`Results: 26 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
