# Task Plan: ISSLDiagnostics Residual Slimming

## Goal
Push the diagnostics owner-path cleanup one step further by moving WinSSL runtime diagnostics tests to `ISSLDiagnostics` and shrinking the remaining direct-core proof to the backend contract only.

## Status
Complete

## Current Plan
- No runtime behavior change was required.
- The old residual note was stale after the WinSSL runtime tests moved to owner path.
- The remaining direct-core diagnostics proof now lives only in `tests/contract/test_backend_contract.pas`.

## Done
- Migrated `tests/winssl/test_winssl_monitoring.pas` to `ISSLDiagnostics`.
- Migrated the diagnostics portions of `tests/winssl/test_winssl_connection_edge_cases.pas` to `ISSLDiagnostics`.
- Updated `tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh` to drop the WinSSL residual runtime allowlist.
- Synced `src/fafafa.ssl.connection.base.pas` and the diagnostics plan doc to the slimmer truth.
- Revalidated the diagnostics contracts and backend contract.

## Verification
- `bash tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh`
- `bash tests/scripts/test_issldiagnostics_active_guidance_contract.sh`
- `bash tests/contract/test_backend_contract.pas` via `/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc` earlier in this session
- `git diff --check`
- `fpc` compile attempts for the two WinSSL test units were blocked on this Linux host because `fafafa.ssl.winssl.certificate` requires the `Windows` unit

## Next
- The next architecture batch should move to the remaining owner clusters or the `TSSLConfig` scope blueprint, whichever shows the freshest contract pressure first.
