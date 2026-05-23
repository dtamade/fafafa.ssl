# Task Plan: MbedTLS Framework Owner-Surface Warning Closeout

## Goal
Close the deprecated-warning drift in `tests/test_mbedtls_framework.pas` by moving its active framework checks onto owner-path SNI / verify-result surfaces and freezing the new truth with focused contracts.

## Status
Complete

## Current Plan
- [docs/plans/2026-05-24-mbedtls-framework-owner-surface-warning-closeout.md](docs/plans/2026-05-24-mbedtls-framework-owner-surface-warning-closeout.md)

## Done
- Migrated `tests/test_mbedtls_framework.pas` from context-level SNI and direct core verify-result mirrors to `ISSLClientConnection` / `ISSLCertificateVerification`.
- Fixed the helper-loss contract's interface lifetime regression by letting the interface own the connection instead of mixing interface refs with manual `Free`.
- Removed `tests/test_mbedtls_framework.pas` from the direct-core verify-result residual allowlists and direct context-SNI classification allowlists.
- Added `tests/scripts/test_mbedtls_framework_owner_surface_contract.sh` to compile the framework test and fail on the exact deprecated warnings that triggered this batch.

## Verification
- `bash tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
- `bash tests/scripts/test_backend_framework_context_level_sni_labels_contract.sh`
- `bash tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
- `bash tests/scripts/test_isslcertificateverification_mbedtls_residual_contract.sh`
- `bash tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
- `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
- `bash tests/scripts/test_mbedtls_framework_owner_surface_contract.sh`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `git diff --check`
