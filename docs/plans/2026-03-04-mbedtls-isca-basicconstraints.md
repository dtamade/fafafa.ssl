# MbedTLS IsCA BasicConstraints Semantics

## Goal
Remove `TMbedTLSCertificate.IsCA` hardcoded `False` behavior and align with certificate BasicConstraints semantics.

## Architecture
- Add RED assertions in `tests/test_mbedtls_framework.pas`:
  - CA fixture must report `IsCA=True`
  - leaf fixture must report `IsCA=False`
- Implement minimal parser fallback in `src/fafafa.ssl.mbedtls.certificate.pas`:
  - `SaveToDER -> TX509Certificate.LoadFromDER -> IsCA`
  - fail-safe `False` on parse/runtime errors

## Files
- Modify: `tests/test_mbedtls_framework.pas`
- Modify: `src/fafafa.ssl.mbedtls.certificate.pas`

## Steps
1. RED
- `fpc -Fu./src tests/test_mbedtls_framework.pas -otmp/test_mbedtls_framework && ./tmp/test_mbedtls_framework`
- Expected: CA fixture assertion fails (current hardcoded `False`).

2. GREEN
- Same command as RED.
- Expected: CA/leaf assertions pass.

3. Regression
- `fpc -Fu./src tests/test_mbedtls_framework.pas -otmp/test_mbedtls_framework && ./tmp/test_mbedtls_framework`
- `fpc -Fu./src -Fu./examples tests/mbedtls/test_mbedtls_cert_chain.pas -otmp/test_mbedtls_cert_chain && ./tmp/test_mbedtls_cert_chain`

