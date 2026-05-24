# Task Plan: Managed Result Initialization Safety Wave 3

## Objective

Close the wave3 managed-result initialization batch for shared TLS 1.3
primitive helpers and constant-time helpers, while keeping the focused
verification gate stable and warning-clean.

## Current State

- Wave 1 and wave 2 are closed and committed.
- Wave3 production targets already satisfy the type-safe initialization
  contract on current head:
  - `CopyBytes(...)`
  - `ConcatBytes(...)`
  - `BuildTLS13HKDFLabel(...)`
  - `HKDF_Expand_SHA256(...)`
  - `HKDF_Expand_SHA384(...)`
  - `TConstantTime.Select(...)`
- The focused constant-time runtime test exposed a flaky wall-clock variance
  assertion unrelated to managed-result initialization.
- This round changed that test to keep deterministic equal/different compare
  loops without using millisecond-resolution jitter as a pass/fail signal.

## Verification

Completed:

- `bash -n tests/scripts/test_managed_result_init_safety_wave3_contract.sh`
- `bash tests/scripts/test_managed_result_init_safety_wave3_contract.sh`
- compile/run `tests/test_tls13_foundation.pas`
- compile/run `tests/unit/test_constant_time.pas`
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

If we continue after this batch, the next likely target is
`docs/plans/2026-05-20-managed-result-init-safety-wave4.md`.
