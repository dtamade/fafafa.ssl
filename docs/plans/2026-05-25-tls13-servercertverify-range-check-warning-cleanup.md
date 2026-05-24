# TLS13 ServerCertVerify Range-Check Warning Cleanup

## Goal

Close the `Warning: Range check error while evaluating constants` family in
`tests/test_tls13_servercertverify.pas` without changing BigInt test semantics.

## Architecture

The warnings are emitted while compiling QWord BigInt vector tests that pass
64-bit hex bit patterns directly as untyped integer constants. Values with the
top bit set are valid unsigned `QWord` inputs, but FreePascal first evaluates
the literal as a signed constant and reports a range-check warning.

The fix is to keep the same test vectors and expected behavior while making the
unsigned intent explicit with `QWord(...)`, matching the already-clean WaveI
style in the same test file.

## Files

- `tests/test_tls13_servercertverify.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. Capture focused red evidence by compiling `tests/test_tls13_servercertverify.pas`
   and grepping for the target range-check warning.
2. Wrap WaveD/WaveF QWord vector literals in explicit `QWord(...)` casts.
3. Recompile the focused target and grep for the target warning.
4. Run the TLS 1.3 completeness gate with a new run id.
5. Update working records, run `git diff --check`, review, and commit.

## Expected Outputs

- Focused compile keeps passing.
- `rg -n "Warning: Range check error while evaluating constants"` returns no
  matches for the new focused compile log.
- TLS 1.3 completeness gate passes with 18 passed, 0 failed.
- Other warning families, if still present, remain explicitly out of scope for
  later batches.
