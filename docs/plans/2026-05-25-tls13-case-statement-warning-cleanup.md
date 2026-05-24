# TLS13 Case-Statement Warning Cleanup

## Goal

Close the TLS 1.3 completeness gate's
`Warning: Case statement does not handle all possible cases` family without
changing runtime test behavior.

## Scope

- `tests/test_tls13_servercertverify.pas`
- `tests/test_freepascal_client_certificateverify_runtime.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Rationale

The residual warnings are all test-harness control-flow warnings:

- PEM key mutation helpers intentionally handle only `pemRSAPrivateKey` and
  `pemPrivateKey`; all other PEM block types should keep returning an empty
  result.
- The scripted CertificateVerify runtime mode has a valid no-op mode.
- The early-data replay store failure modes are operation-specific; modes for
  other operations should fall through to the normal success path.

## Verification

```bash
/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/case_wave1_green -FEtmp/case_wave1_green tests/test_tls13_servercertverify.pas
/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/case_wave1_green -FEtmp/case_wave1_green tests/test_freepascal_client_certificateverify_runtime.pas
/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/case_wave1_green -FEtmp/case_wave1_green tests/test_freepascal_tls13_early_data.pas
rg -n "Warning: Case statement does not handle all possible cases" tmp/case_wave1_*_compile_green.log
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id case_wave1_20260525 --fpc-exe /opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc
git diff --check
```
