# TLS13 Source Note Cleanup Wave1

## Goal

Close the repeated compiler `Note:` families exposed by the TLS 1.3
completeness gate by cleaning source-level unused locals in shared production
units, while preserving runtime behavior.

## Scope

- `src/fafafa.ssl.openssl.api.store.pas`
- `src/fafafa.ssl.pkcs11.provider.pas`
- `src/fafafa.ssl.cert.pinning.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Rationale

The latest TLS 1.3 completeness gate shows repeated source note families in the
shared backend/provider units, not in the tests:

- `PasswordAnsi` in `openssl.api.store`
- `Cert` in `openssl.api.store`
- `PINAnsi` in `pkcs11.provider`
- `PIN` in `pkcs11.provider`
- `Pin` in `cert.pinning`
- `LParts` in `wolfssl.lib`

The first four are straightforward dead locals or dead conversions.
`LParts` is a leftover placeholder in version parsing.
`cert.pinning` needs a careful read so we keep the hash-copy semantics intact
while removing the unused local warning path.

## Steps

1. Remove the dead locals and placeholder state in the four source units.
2. Recompile the affected units and grep the compile logs for the target
   `Note:` strings.
3. Run the TLS 1.3 completeness gate with a fresh run id.
4. Update the working records, review, and commit.

## Verification

```bash
rg -n "Note:" tmp/warning_clearance_20260525_tls13_completeness.log
/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -FUtmp/source_note_wave1_red_units -FEtmp/source_note_wave1_red_bin src/fafafa.ssl.openssl.api.store.pas
/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -FUtmp/source_note_wave1_red_units -FEtmp/source_note_wave1_red_bin src/fafafa.ssl.pkcs11.provider.pas
/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -FUtmp/source_note_wave1_red_units -FEtmp/source_note_wave1_red_bin src/fafafa.ssl.cert.pinning.pas
/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -FUtmp/source_note_wave1_red_units -FEtmp/source_note_wave1_red_bin src/fafafa.ssl.wolfssl.lib.pas
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id source_note_wave1_20260525 --fpc-exe /opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc
git diff --check
```

## Execution Result

- Focused unit compiles for:
  - `src/fafafa.ssl.openssl.api.store.pas`
  - `src/fafafa.ssl.pkcs11.provider.pas`
  - `src/fafafa.ssl.cert.pinning.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  are clean for the targeted source note families.
- `python3 scripts/compile_all_modules.py --rebuild ...` completed `186/186`
  source modules with `0` warnings.
- TLS 1.3 completeness gate passed with `18` tests passed and `0` failed.
- The gate still shows test-harness `Note:` output in
  `test_freepascal_tls13_early_data.pas`; that is now the next batch, not part
  of this source batch.
