# Task Plan: Module Test Unreachable-Code Warning Cleanup

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
