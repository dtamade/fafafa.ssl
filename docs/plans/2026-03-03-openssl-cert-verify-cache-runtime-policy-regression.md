# OpenSSL Cert Verify Cache Runtime Policy Regression (2026-03-03)

## Goal
Add a real runtime regression contract (not source-string inspection) to verify that OpenSSL certificate-verify cache valid-hit policy branches are actually hit during handshake flow:
- `skip X509_verify_cert` when `ssoSkipCertVerifyCacheValidHitRefresh` is enabled
- `refresh X509_verify_cert` when the option is disabled

## Architecture / Scope
- Add a network-gated integration test that performs two real TLS handshakes to the same host.
- Enable `sslCertVerifyCheckOCSP` so the runtime path in `TOpenSSLConnection.ValidatePostHandshake` executes cache policy logic.
- Capture `TSecurityLog` debug output with an in-memory logger and assert branch markers.
- Keep production code unchanged unless runtime contract exposes a bug.

## Files
- Add: `tests/integration/test_openssl_cert_verify_cache_policy_runtime.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (TDD: RED -> GREEN -> Regression)
1. RED: add runtime contract test file with log-capture assertions for both policy modes.
2. RED verify: compile and run new test with network gate on.
3. GREEN: fix test/runtime integration issues (or production behavior if contract reveals a bug) until test passes.
4. Regression:
   - `fpc -Fu./src -Fu./src/openssl -Fu./tests/framework -Fi./src tests/integration/test_openssl_cert_verify_cache_policy_runtime.pas -otmp/test_openssl_cert_verify_cache_policy_runtime && FAFAFA_RUN_NETWORK_TESTS=1 ./tmp/test_openssl_cert_verify_cache_policy_runtime`
   - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/openssl/test_openssl_cert_verify_cache_policy.pas -otmp/test_openssl_cert_verify_cache_policy && ./tmp/test_openssl_cert_verify_cache_policy`
   - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/openssl/test_openssl_features.pas -otmp/test_openssl_features && ./tmp/test_openssl_features`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Runtime test can assert valid-hit policy branch by captured logs.
- Skip-policy scenario hits:
  - `Cert verify cache hit (valid result), skipping X509_verify_cert`
- Refresh-policy scenario hits:
  - `Cert verify cache hit (valid result), refreshing X509_verify_cert`
- Injected invalid-cache scenario hits:
  - `Cert verify cache hit (invalid result), refreshing X509_verify_cert`
  - and should not hit `Cert verify cache hit (invalid result), skipping X509_verify_cert` in issuer-resolved path
- Focused regressions and full module compile gate remain green.
