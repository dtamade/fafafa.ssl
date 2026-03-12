# Cert Verify Cache TTL=0 Fail-Closed Semantics

## Goal
Ensure `TCertVerifyCache` treats non-positive TTL as immediate expiry (fail-closed). `TTL=0` must not return cache hits.

## Architecture
- RED: extend nil-guard contract to assert `Create(..., TTL=0)` cannot produce a hit after `Put`.
- GREEN: minimal change in `src/fafafa.ssl.cert.verify.cache.pas`:
  - `IsExpired` returns true when `FTTL<=0`
  - use inclusive boundary `SecondsBetween(...) >= FTTL` for positive TTLs.
- Regression: concurrency contract + OpenSSL cache policy contract + compile gate.

## Files
- Modify: `tests/test_cert_verify_cache_nil_guard.pas`
- Modify: `src/fafafa.ssl.cert.verify.cache.pas`

## Steps
1. RED
- `fpc -Fu./src -Fu./src/openssl -Fi./src tests/test_cert_verify_cache_nil_guard.pas -otmp/test_cert_verify_cache_nil_guard && ./tmp/test_cert_verify_cache_nil_guard`
- Expected: `TTL=0` contract fails.

2. GREEN
- Same command as RED.
- Expected: contract passes.

3. Regression
- `fpc -Fu./src -Fu./src/openssl -Fi./src tests/test_cert_verify_cache_concurrency.pas -otmp/test_cert_verify_cache_concurrency && ./tmp/test_cert_verify_cache_concurrency`
- `fpc -Fu./src -Fu./src/openssl -Fi./src tests/openssl/test_openssl_cert_verify_cache_policy.pas -otmp/test_openssl_cert_verify_cache_policy && ./tmp/test_openssl_cert_verify_cache_policy`
- `python3 scripts/compile_all_modules.py`

## Execution Log (2026-03-04)

### RED
- Modified: `tests/test_cert_verify_cache_nil_guard.pas`
  - Added `ZeroTTLCache := TCertVerifyCache.Create(8, 0)` contract:
    - put valid cert
    - `TryGet` must return `False`

- Command:
  - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/test_cert_verify_cache_nil_guard.pas -otmp/test_cert_verify_cache_nil_guard && ./tmp/test_cert_verify_cache_nil_guard`
- RED result (key):
  - `Exception: TTL=0 cache must not return a hit`

### GREEN
- Modified: `src/fafafa.ssl.cert.verify.cache.pas`
  - `IsExpired`:
    - add `FTTL<=0 => expired`
    - change comparison from `>` to `>=`

- Command:
  - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/test_cert_verify_cache_nil_guard.pas -otmp/test_cert_verify_cache_nil_guard && ./tmp/test_cert_verify_cache_nil_guard`
- GREEN result:
  - `✅ cert verify cache nil guard contract passed`

### Regression
- `fpc -Fu./src -Fu./src/openssl -Fi./src tests/test_cert_verify_cache_concurrency.pas -otmp/test_cert_verify_cache_concurrency && ./tmp/test_cert_verify_cache_concurrency`
  - PASS (`✅ cert verify cache concurrency contract passed`)
- `fpc -Fu./src -Fu./src/openssl -Fi./src tests/openssl/test_openssl_cert_verify_cache_policy.pas -otmp/test_openssl_cert_verify_cache_policy && ./tmp/test_openssl_cert_verify_cache_policy`
  - PASS (`Total tests: 7, Passed: 7, Failed: 0`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
