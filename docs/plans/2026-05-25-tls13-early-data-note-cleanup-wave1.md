# TLS13 Early-Data Note Cleanup Wave1

## Goal

Close the remaining compiler `Note:` families in
`tests/test_freepascal_tls13_early_data.pas` without changing the early-data or
replay-store test semantics.

## Scope

- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Rationale

The TLS 1.3 completeness gate now only shows test-harness notes in the
early-data file:

- `LTicketNonce` / `LTicket` in `TScriptedEarlyDataServerStream.HandleClientHello`
- `LCount` in the replay-store helper writers
- `TRAILING_GARBAGE_BYTES` in the replay-store trailing-garbage helper
- `DIRECTORY_TRAILING_GARBAGE_BYTES` in the directory replay-store helper
- `LVersion` in the corrupt-directory helper

`HandleClientHello` has a true dead local pair.
The replay-store helpers are test fixture writers; the count and garbage bytes
can be expressed more directly without changing the bytes written to disk.

## Steps

1. Remove the dead locals in `HandleClientHello`.
2. Rewrite the replay-store fixture writers to avoid the note-triggering locals.
3. Recompile `tests/test_freepascal_tls13_early_data.pas` and grep the target
   note strings.
4. Run the TLS 1.3 completeness gate with a fresh run id.
5. Update the working records, review, and commit.

## Verification

```bash
/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/early_data_note_wave1_green -FEtmp/early_data_note_wave1_green tests/test_freepascal_tls13_early_data.pas
rg -n "Note:" tmp/early_data_note_wave1_early_data_compile_green.log
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_note_wave1_20260525 --fpc-exe /opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc
git diff --check
```

## Execution Result

- Focused compile for `tests/test_freepascal_tls13_early_data.pas` is clean for
  the targeted note families.
- TLS 1.3 completeness gate passed with `18` tests passed and `0` failed.
- `rg -n "Note:" tmp/early_data_note_wave1_tls13_completeness.log` returned no
  matches.
