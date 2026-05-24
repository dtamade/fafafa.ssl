# Task Plan: TLS13 ServerCertVerify Range-Check Warning Cleanup

## Objective

Close the `Warning: Range check error while evaluating constants` family in
`tests/test_tls13_servercertverify.pas` without changing BigInt test semantics.

## Current State

- Focused red compile showed 62 warnings, with 47 range-check hits clustered in
  `TestBigIntQWordVectorSuiteWaveD/F`.
- The fix was to make the unsigned intent explicit with `QWord(...)` casts on
  the 64-bit vector literals, matching the already-clean WaveI style.
- Focused compile for `tests/test_tls13_servercertverify.pas` is clean for the
  target range-check warning family.
- The target binary passed.
- The TLS 1.3 completeness gate passed with `18` passed and `0` failed.
- Remaining gate warnings are different families:
  `Case statement does not handle all possible cases`,
  implicit string conversion, and one `Function result does not seem to be set`.

## Verification

- Focused red evidence:
  - `tests/test_tls13_servercertverify.pas` compiled with 62 warnings before the
    fix, including the range-check family at lines `3109-3168`.
- Focused green evidence:
  - `tmp/range_check_wave1_servercertverify_compile_green.log`
  - `tmp/range_check_wave1_servercertverify_run.log`
- Full gate evidence:
  - `tmp/range_check_wave1_tls13_completeness.log`
  - `tmp/test-reports/freepascal_tls13_completeness_range_check_wave1_20260525.md`

## Next Queue

- Treat the remaining `case statement`, string conversion, and
  `function result` warnings as separate waves.

## Objective

Close the module-test `Warning: Unreachable code` batch exposed by
`run_all_module_tests.sh --fast-local`.

## Current State

- Managed-result warning cleanup is closed on current evidence.
- Broad module-test compile logs then exposed test `Unreachable code` warnings
  in constant assertion blocks.
- Root cause: FPC folds direct compile-time constant comparisons and marks the
  failure branch unreachable.
- Target tests now keep the same expected values and failure messages but route
  constant operands through local `RuntimeInteger` helpers.
- Focused compile for all 9 target files is clean for
  `Warning: Unreachable code`.
- Full module test gate passes and broad module-test compile logs contain no
  `Warning:` entries.

## Verification

Completed:

- focused compile for:
  - `tests/certificate/test_p2_pkcs12.pas`
  - `tests/certificate/test_p2_ocsp.pas`
  - `tests/certificate/test_p2_ts.pas`
  - `tests/certificate/test_p2_cms.pas`
  - `tests/certificate/test_p2_ct.pas`
  - `tests/certificate/test_p2_pkcs7.pas`
  - `tests/crypto/test_p2_store.pas`
  - `tests/crypto/test_p2_comp.pas`
  - `tests/crypto/test_p4_engine.pas`
- `rg -n "Warning: Unreachable code" tmp/unreachable_code_wave1_*_compile.log`
  - result: no matches
- `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc FAFAFA_FAST_LOCAL=1 FAFAFA_FPC_UNIT_OUTPUT_DIR=tmp/unreachable_code_wave1_module_units bash scripts/run_all_module_tests.sh --fast-local`
  - result: `22` passed, `0` failed, `0` skipped
- `rg -n "Warning:" tmp/test-reports/*20260524_235928_1700710*_compile.log`
  - result: no matches

Completed after verification:

- `git diff --check`
- final review
- commit `6ac93d7 test: clear module unreachable-code warnings`

## Per-Round Contract

Each round must have:

- One named target batch.
- Focused contract verification before broader compile checks.
- `git diff --check`.
- Brief review conclusion before commit.
- Git commit after the round.

## Next Round

After committing this batch, continue from fresh compile evidence. Keep TLS 1.3
case/range/string-conversion warnings separate from this module-test constants
cleanup.
