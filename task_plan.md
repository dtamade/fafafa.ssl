# Task Plan: Managed Result Initialization Safety Wave 4

## Objective

Close the wave4 managed-result initialization batch for shared TLS 1.3
key-schedule and ClientHello builder helpers, while keeping verification
focused on the runtime paths that compile those units.

## Current State

- Wave 1, wave 2, and wave 3 are closed and committed.
- Wave4 production targets already satisfy the type-safe initialization
  contract on current head:
  - `HashTranscriptForSuite(...)`
  - `HKDFExtractForSuite(...)`
  - `HKDFExpandLabelForSuite(...)`
  - `TLS13ComputePSKBinderForCipherSuite(...)`
  - `BuildExtensionServerName(...)`
  - `BuildExtensionALPN(...)`
  - `BuildExtensionPreSharedKey(...)`
  - `BuildTLS13ClientHelloBody(...)`
  - `BuildTLS13ClientHelloBodyWithPSKCore(...)`
  - `BuildTLS13ClientHelloHandshake(...)`
  - `BuildTLS13ClientHelloHandshakeWithPSK(...)`
  - `BuildTLS13ClientHelloHandshakeWithComputedPSKBinder(...)`
- The focused compile logs rebuilt both `tls13.keyschedule` and
  `tls13.clienthello` without the managed-result warning class.
- No production code edits were required in this batch.

## Verification

Completed:

- `bash -n tests/scripts/test_managed_result_init_safety_wave4_contract.sh`
- `bash tests/scripts/test_managed_result_init_safety_wave4_contract.sh`
- compile/run `tests/test_tls13_foundation.pas`
- compile/run `tests/test_tls13_resumption.pas`
- compile-log grep for `Warning: Function result variable of a managed type`

Pending before commit:

- `git diff --check`
- final review and commit

## Per-Round Contract

Each round must have:

- One named target batch.
- Focused contract verification before broader compile checks.
- `git diff --check`.
- Brief review conclusion before commit.
- Git commit after the round.

## Next Round

If we continue after this batch, the next target is
`docs/plans/2026-05-20-managed-result-init-safety-wave5.md`.
