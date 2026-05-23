# Findings

## Conclusions
- `tests/test_mbedtls_framework.pas` no longer belongs in the active direct context-SNI or direct-core verify-result residual clusters; its framework coverage now follows `ISSLClientConnection` and `ISSLCertificateVerification`.
- The real seam in this batch was compile-time drift, not product logic drift: focused compilation of `tests/test_mbedtls_framework.pas` emitted 5 deprecated warnings before the change.
- The only regression introduced during the migration was an interface-lifetime bug in `TestMbedTLSVerifyResultHelperLossContract`; handing ownership to `ISSLConnection` and removing manual `Free` restored runtime stability.

## Notes
- `TMbedTLSConnection.DoGetVerifyResultString` still carries `FLastErrorString` for non-verification failures such as unsupported renegotiation; this batch preserved that semantic truth but moved access to the `ISSLCertificateVerification` owner path.
- The new guard is `tests/scripts/test_mbedtls_framework_owner_surface_contract.sh`; it compiles the framework test and rejects reintroduced deprecated warnings for context-level SNI and direct-core verify-result mirrors.
