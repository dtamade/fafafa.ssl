# Cert Verify Cache Zero-Capacity Crash Guard

## Goal
Prevent `TCertVerifyCache` from out-of-bounds writes when created with non-positive capacity (`0` or negative). Non-positive capacity should behave as disabled cache without crashes.

## Architecture
- RED: extend nil-guard contract test to cover `Create(0, ...)` + `Put(validCert, ...)`.
- GREEN: minimal hardening in `src/fafafa.ssl.cert.verify.cache.pas`:
  - clamp constructor capacity to `>=0`
  - short-circuit `Put` when `FCapacity<=0`
- Regression: run cache concurrency contract + OpenSSL policy contract + full compile gate.

## Files
- Modify: `tests/test_cert_verify_cache_nil_guard.pas`
- Modify: `src/fafafa.ssl.cert.verify.cache.pas`

## Steps
1. RED
- `fpc -Fu./src -Fu./src/openssl -Fi./src tests/test_cert_verify_cache_nil_guard.pas -otmp/test_cert_verify_cache_nil_guard && ./tmp/test_cert_verify_cache_nil_guard`
- Expected: runtime crash or failing assertion in zero-capacity path.

2. GREEN
- Same command as RED.
- Expected: nil-guard contract and zero-capacity contract both pass.

3. Regression
- `fpc -Fu./src -Fu./src/openssl -Fi./src tests/test_cert_verify_cache_concurrency.pas -otmp/test_cert_verify_cache_concurrency && ./tmp/test_cert_verify_cache_concurrency`
- `fpc -Fu./src -Fu./src/openssl -Fi./src tests/openssl/test_openssl_cert_verify_cache_policy.pas -otmp/test_openssl_cert_verify_cache_policy && ./tmp/test_openssl_cert_verify_cache_policy`
- `python3 scripts/compile_all_modules.py`

## Execution Log (2026-03-04)

### RED
- Modified: `tests/test_cert_verify_cache_nil_guard.pas`
  - Added PEM cert loader (`signer_cert.pem`) for real `Put` path.
  - Added contract: `TCertVerifyCache.Create(0, ...)` + `Put(Cert, VerifyResult)` must keep size `0`.

- Command:
  - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/test_cert_verify_cache_nil_guard.pas -otmp/test_cert_verify_cache_nil_guard && ./tmp/test_cert_verify_cache_nil_guard`
- RED result (key):
  - Runtime crash:
    - `EAccessViolation: Access violation`

### GREEN
- Modified: `src/fafafa.ssl.cert.verify.cache.pas`
  - `Create`:
    - clamp `ACapacity` to `0` when non-positive.
  - `Put`:
    - early-exit when `FCapacity<=0`.

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
