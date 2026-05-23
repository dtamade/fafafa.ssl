# Task Plan: MbedTLS OCSP Capability Doc Truth Resync

## Goal
Keep the MbedTLS OCSP capability contract aligned with current fail-closed source truth without reopening a non-existent online OCSP path.

## Status
Complete

## Current Plan
- No runtime or public API change was required.
- The false-red came from treating any `sslCertVerifyCheckOCSP` mention in MbedTLS source as an online OCSP publication signal.
- The contract now preserves the fail-closed VerifyEx branch and forbids actual online OCSP helper publication.

## Done
- Updated `tests/scripts/test_mbedtls_ocsp_capability_doc_truth_contract.sh`.
- Revalidated the MbedTLS OCSP capability contract.
- Verified diff hygiene with `git diff --check`.

## Verification
- `bash tests/scripts/test_mbedtls_ocsp_capability_doc_truth_contract.sh`
- `git diff --check`

## Next
- Do not reopen `TSSLConfig`, `GetContext`, `GetStateString`, or `GetSelectedALPNProtocol` unless a fresh focused contract goes red.
- The next architecture batch should move to the remaining owner clusters: diagnostics, session-resumption, certificate-verification, or OCSP.
