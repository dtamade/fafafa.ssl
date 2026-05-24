# Task Plan: Managed Result Initialization Safety Wave 6

## Objective

Close the wave6 managed-result initialization batch for TLS 1.3 completeness
test harness helpers exposed by residual discovery.

## Current State

- Waves 1 through 5 are closed and committed.
- Residual discovery proved source modules are clean for this warning class:
  `scripts/compile_all_modules.py --rebuild` compiled `186/186` source
  modules with `0` warnings.
- The TLS 1.3 completeness gate still exposed managed-result initialization
  warnings in 11 test harness files.
- Wave6 updates only test helper initialization style:
  `SetLength(Result, 0)` empty dynamic-array initialization was replaced with
  `Result := nil`, and direct `SetLength(Result, ...)` paths now explicitly
  initialize `Result` first where needed.
- Full TLS 1.3 completeness gate now passes and the wave6 gate log contains no
  `Warning: Function result variable of a managed type does not seem to be initialized`.

## Verification

Completed:

- `bash -n tests/scripts/test_managed_result_init_safety_wave6_contract.sh`
- `bash tests/scripts/test_managed_result_init_safety_wave6_contract.sh`
- `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id managed_result_wave6_tls13 --fpc-exe /opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc`
  - report: `tmp/test-reports/freepascal_tls13_completeness_managed_result_wave6_tls13.md`
  - result: `18` passed, `0` failed
- `rg -n "Warning: Function result variable of a managed type does not seem to be initialized" tmp/managed_result_wave6_tls13_completeness.log`
  - result: no matches

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

After committing wave6, run a small residual-discovery pass. Open wave7 only if
fresh compile or gate evidence identifies concrete remaining managed-result
warnings; otherwise switch to the next verified high-value warning family.
