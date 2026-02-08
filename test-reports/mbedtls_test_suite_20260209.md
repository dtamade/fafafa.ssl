# MbedTLS Test Suite Report

- **Date**: 2026-02-09
- **MbedTLS Version**: 3.6.5
- **Platform**: Linux x86_64

## Summary

| Metric | Value |
|--------|-------|
| Total Tests | 20 |
| Passed | 20 |
| Failed | 0 |
| Pass Rate | **100%** |

## Test Results

| Test | Status |
|------|--------|
| test_mbedtls_alpn | ✅ PASS |
| test_mbedtls_backend | ✅ PASS |
| test_mbedtls_basic | ✅ PASS |
| test_mbedtls_cert_chain | ✅ PASS |
| test_mbedtls_cert_errors | ✅ PASS |
| test_mbedtls_cert_verify_flags | ✅ PASS |
| test_mbedtls_connection | ✅ PASS |
| test_mbedtls_connection_final | ✅ PASS |
| test_mbedtls_context_only | ✅ PASS |
| test_mbedtls_correct_order | ✅ PASS |
| test_mbedtls_interface_lifecycle | ✅ PASS |
| test_mbedtls_interface_only | ✅ PASS |
| test_mbedtls_lib_free | ✅ PASS |
| test_mbedtls_lowlevel | ✅ PASS |
| test_mbedtls_ocsp_capability | ✅ PASS |
| test_mbedtls_safe | ✅ PASS |
| test_mbedtls_server_accept | ✅ PASS |
| test_mbedtls_server_accept_simple | ✅ PASS |
| test_mbedtls_server_context | ✅ PASS |
| test_mbedtls_simple_connection | ✅ PASS |

## Key Fixes Applied

1. **MbedTLS 3.x MD Type Constants** (commit `90cb5ba`)
   - SHA224: 5 → 8
   - SHA256: 6 → 9
   - SHA384: 7 → 10
   - SHA512: 8 → 11
   - Added SHA3 constants (16-19)

2. **Certificate Fingerprint Fix**
   - GetFingerprintSHA1/SHA256 now correctly access native handle's raw DER data
   - Peer certificates from connections now work correctly

## Capabilities Verified

- TLS 1.2: ✅
- TLS 1.3: ✅
- ALPN: ✅
- SNI: ✅
- Session Tickets: ✅
- ECDHE: ✅
- ChaCha20-Poly1305: ✅
- Certificate Verification: ✅
- Certificate Fingerprinting: ✅
- Server Accept: ✅

## Known Limitations

- OCSP Stapling: Not supported (MbedTLS client limitation)
- Certificate Transparency: Not supported
