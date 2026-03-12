# MbedTLS Certificate Date Unknown Semantics

## Goal
Eliminate fake default certificate validity timestamps in `TMbedTLSCertificate` (`Now-365` / `Now+365`) and align unknown-state behavior with explicit semantics (`0` for unknown dates).

## Architecture
- Add RED assertions in framework-level tests for an empty `TMbedTLSCertificate`.
- Implement minimal behavior changes:
  - `GetNotBefore` returns `0` when unavailable/unparsable.
  - `GetNotAfter` returns `0` when unavailable/unparsable.
  - `IsExpired` returns `False` when `NotAfter` is unknown.
  - `GetDaysUntilExpiry` returns `0` when `NotAfter` is unknown.
- Run focused regressions in MbedTLS test surface.

## Files
- Modify: `tests/test_mbedtls_framework.pas`
- Modify: `src/fafafa.ssl.mbedtls.certificate.pas`

## Step-by-step
1. RED
- Command:
  - `fpc -Fu./src tests/test_mbedtls_framework.pas -otmp/test_mbedtls_framework && ./tmp/test_mbedtls_framework`
- Expected:
  - newly added unknown-semantics assertions fail with current fake defaults.

2. GREEN
- Command:
  - same as RED
- Expected:
  - unknown-semantics assertions pass.

3. Regression
- Commands:
  - `fpc -Fu./src tests/test_mbedtls_framework.pas -otmp/test_mbedtls_framework && ./tmp/test_mbedtls_framework`
  - `fpc -Fu./src -Fu./examples tests/mbedtls/test_mbedtls_cert_chain.pas -otmp/test_mbedtls_cert_chain && ./tmp/test_mbedtls_cert_chain`
- Expected:
  - both pass.
