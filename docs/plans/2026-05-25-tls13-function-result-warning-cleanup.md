# TLS13 Function-Result Warning Cleanup

## Goal

Close the remaining `Warning: Function result does not seem to be set` family
in the TLS 1.3 completeness gate.

## Scope

- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Rationale

`TExplodingReplayProviderStore.TryAcquireReplayKey` is intentionally fail-closed:
it always raises an exception. FreePascal still warns because the Boolean result
is never assigned on the visible control-flow path. Setting `Result := False`
before the raise preserves the behavior and silences the warning.

## Verification

```bash
/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/function_wave1_green -FEtmp/function_wave1_green tests/test_freepascal_tls13_early_data.pas
rg -n "Warning: Function result does not seem to be set" tmp/function_wave1_early_data_compile_green.log
tmp/function_wave1_green/test_freepascal_tls13_early_data
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id function_wave1_20260525 --fpc-exe /opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc
git diff --check
```
