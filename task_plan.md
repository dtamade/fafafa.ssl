# Task Plan: ISSLSessionResumption Residual Slimming

## Goal
Move the remaining backend semantic session-resumption proofs to `ISSLSessionResumption` owner path and shrink direct-core session-resumption residuals to the backend contract mirror proof only.

## Status
Complete

## Current Plan
- No public API or runtime implementation change was required.
- The MbedTLS and OpenSSL semantic tests can prove the same behavior through `ISSLSessionResumption`.
- The remaining direct-core session-resumption proof now lives only in `tests/contract/test_backend_contract.pas`.

## Done
- Migrated `tests/test_mbedtls_connection_session_reused_contract.pas` to `ISSLSessionResumption`.
- Migrated `tests/test_openssl_connection_session_reused_contract.pas` to `ISSLSessionResumption`.
- Fixed the interface/manual-free lifetime issue exposed by the migration.
- Updated session-resumption residual contracts and source comments to the slimmer truth.
- Updated the session-resumption plan closeout.

## Verification
- `bash tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh`
- `bash tests/scripts/test_isslsessionresumption_compiler_deprecated_contract.sh`
- `bash tests/scripts/test_isslsessionresumption_runtime_owner_path_contract.sh`
- `/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc ... tests/test_mbedtls_connection_session_reused_contract.pas`
- `./tmp/test_mbedtls_connection_session_reused_contract/test_mbedtls_connection_session_reused_contract`
- `/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc ... tests/test_openssl_connection_session_reused_contract.pas`
- `./tmp/test_openssl_connection_session_reused_contract/test_openssl_connection_session_reused_contract`
- `git diff --check`

## Next
- The next highest-value owner cluster is likely `ISSLCertificateVerification`, using the same standard: do not preserve direct-core residuals unless a focused contract proves they must stay.
