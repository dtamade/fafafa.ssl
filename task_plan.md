# Task Plan: Managed Result Initialization Safety Wave 2

## Objective

Close the next managed-result initialization safety batch by keeping shared TLS
1.3/session implementation code and its focused verification harness free of
`SetLength(Result, 0)` on uninitialized managed `TBytes` results.

## Current State

- Wave 1 is closed and committed.
- Wave 2 production functions are already in the intended type-safe shape:
  - `BuildTLSPlaintext(...)`
  - `ReadVector16(...)`
  - `TFreePascalSession.Serialize(...)`
- The focused session-resumption compile exposed the live residual warning class
  in `tests/test_freepascal_client_session_resumption.pas`.
- This round changed that harness to use `Result := nil` in all helper functions
  that return empty/build-up `TBytes` results.
- `tests/scripts/test_managed_result_init_safety_wave2_contract.sh` now guards
  both production functions and the session-resumption harness helpers.

## Verification

Completed:

- `bash -n tests/scripts/test_managed_result_init_safety_wave2_contract.sh`
- `bash tests/scripts/test_managed_result_init_safety_wave2_contract.sh`
- compile/run `tests/test_tls13_foundation.pas`
- compile/run `tests/test_freepascal_client_session_resumption.pas`
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
`docs/plans/2026-05-20-managed-result-init-safety-wave3.md`
(`src/fafafa.ssl.tls13.primitives.pas` /
`src/fafafa.ssl.crypto.constant_time.pas`).
