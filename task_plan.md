# Task Plan: ISSLCertificateVerification Root Residual Slimming

## Goal

Continue shrinking direct-core `GetVerifyResult` / `GetVerifyResultString`
usage by moving ordinary tests to `ISSLCertificateVerification` owner path,
while preserving intentional backend/runtime mirror proofs behind focused
contracts.

## Status

Complete

## Current Plan

- Start with low-risk root tests that only inspect failure text and do not prove
  core mirror equivalence.
- Keep backend-specific runtime contracts frozen unless a focused inspection
  shows they can use the owner interface without weakening their purpose.
- Update residual allowlists immediately after each migration so the contracts
  keep reflecting the smaller public surface.

## This Batch

- Migrate `tests/test_freepascal_client_ct_sct_surface.pas` CT/SCT
  fail-closed text checks from direct `ISSLConnection.GetVerifyResultString` to
  `ISSLCertificateVerification.GetVerifyResultString`.
- Remove the file from the root residual allowlist and compiler-deprecated
  quarantine list.
- Compile and run the migrated Pascal test.

## Verification

- `bash tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
- `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
- `bash tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
- `bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
- `/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_ct_sct_owner/units -FEtmp/test_freepascal_ct_sct_owner/bin tests/test_freepascal_client_ct_sct_surface.pas`
- `tmp/test_freepascal_ct_sct_owner/bin/test_freepascal_client_ct_sct_surface`
- `git diff --check`

## Next

- Inspect `tests/test_freepascal_client_ocsp_stapling_runtime.pas` next; it
  appears to be another single-purpose `GetVerifyResultString` runtime check
  and may be safe to migrate to `ISSLCertificateVerification`.
- Keep `tests/contract/test_backend_contract.pas` as the mirror-proof boundary
  unless the public API deprecation/removal strategy changes.
