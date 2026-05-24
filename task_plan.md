# Task Plan: Managed Result Initialization Safety Wave 5

## Objective

Close the wave5 managed-result initialization batch for shared TLS 1.3
application-schedule helpers, ServerHello builders, and the focused resumption
test helper.

## Current State

- Waves 1 through 4 are closed and committed.
- Wave5 production and test targets already satisfy the type-safe
  initialization contract on current head:
  - `TLS13ComputeResumptionMasterSecretFromTranscriptHash(...)`
  - `TLS13DeriveResumptionPSKFromTranscriptHash(...)`
  - `HashTranscriptForSuite(...)`
  - `HKDFExtractForSuite(...)`
  - `HKDFExpandLabelForSuite(...)`
  - `BuildExtensionHeader(...)`
  - `BuildTLS13ServerHelloBody(...)`
  - `BuildTLS13ServerHelloHandshake(...)`
  - `BuildTLS13ServerHelloHandshakeWithSelectedPSK(...)`
  - `HexToBytes(...)`
- The focused compile logs rebuilt `tls13.appschedule`,
  `tls13.serverhello`, and `tests/test_tls13_resumption.pas` without the
  managed-result warning class.
- No production code edits were required in this batch.

## Verification

Completed:

- `bash -n tests/scripts/test_managed_result_init_safety_wave5_contract.sh`
- `bash tests/scripts/test_managed_result_init_safety_wave5_contract.sh`
- compile/run `tests/test_tls13_appschedule.pas`
- compile/run `tests/test_tls13_serverhello_builder.pas`
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

No `wave6` plan or contract exists on current head. If we continue after this
batch, run a small residual-discovery pass first, then open a follow-up wave
only if compile evidence identifies concrete remaining managed-result warnings.
