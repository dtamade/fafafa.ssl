# Task Plan: Managed Result Initialization Safety

## Objective

Eliminate the remaining Pascal managed-result initialization warnings in the
public facade / shared connection base / verification harness by using
type-safe empty-result initialization instead of `FillChar(...)` or
`SetLength(Result, 0)` on uninitialized managed results.

## Current State

- `src/fafafa.ssl.pas` already uses `Default(TSSLConfig)` for the default-config
  fallback.
- `src/fafafa.ssl.connection.base.pas` already uses `Default(...)` / `Result := nil`
  on the core paths that matter for this batch.
- The remaining live empty-`TBytes` helpers in the verification harness were in
  `tests/test_connection_builder_hostname_precedence.pas`; they now use
  `Result := nil`.
- `tests/scripts/test_managed_result_init_safety_contract.sh` now checks both
  the source contract and the harness helpers.

## Remaining Queue

None for this batch if verification stays green.

## Per-Round Contract

Each round must have:

- One named target file or one explicit freeze decision.
- A short pre-edit classification.
- Focused contract verification before broader compile checks.
- `git diff --check`.
- Brief review conclusion before commit.
- Git commit after the round.

## Stop Conditions

Stop this batch when:

- the managed-result contract passes,
- the two focused Pascal compiles pass,
- and the worktree is clean except for the intended batch changes.

## Next Round

If anything regresses, re-open the exact function that reintroduced the
managed-result warning instead of widening scope.

If we continue after this batch, the next likely target is
`docs/plans/2026-05-20-managed-result-init-safety-wave2.md`
(`src/fafafa.ssl.tls13.wire.pas` / `src/fafafa.ssl.freepascal.session.pas`).
