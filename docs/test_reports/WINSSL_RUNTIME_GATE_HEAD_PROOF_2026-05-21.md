# WinSSL Runtime Gate Head Proof (2026-05-21)

## Summary

- validated head: `80b3500bc00eb3778dff2c97168e61c236e6506b`
- workflow: `WinSSL Runtime Gate`
- event: `workflow_dispatch`
- run id: `26193849105`
- job id: `77068605599`
- run url: `https://github.com/dtamade/fafafa.ssl/actions/runs/26193849105`
- conclusion: `SUCCESS`
- duration: `6m45s`

## Steps

All workflow steps completed successfully:

1. `Install dependencies`
2. `Check Windows version`
3. `Run quick WinSSL smoke`
4. `Run Windows Wave B gate`
5. `Run broader WinSSL runtime suite`
6. `Generate WinSSL workflow summary`
7. `Upload WinSSL evidence`

## Artifact

- artifact name: `winssl-windows-evidence-gh_26193849105_1`
- artifact expired: `false`
- artifact size: `8104` bytes

Downloaded files:

- `winssl_quick_smoke_gh_26193849105_1.log`
- `wave_b_windows_gate_summary_gh_26193849105_1.md`
- `wave_b_windows_winssl_gh_26193849105_1.log`
- `wave_b_windows_openssl_gh_26193849105_1.log`
- `wave_b_windows_modules_gh_26193849105_1.log`
- `validate_all_modules_report_gh_26193849105_1.md`
- `validate_all_modules_compile_gh_26193849105_1.log`
- `winssl_runtime_suite_gh_26193849105_1.log`

## Wave B Summary

`wave_b_windows_gate_summary_gh_26193849105_1.md` reports:

- `overall: PASS`
- `winssl: PASS`
- `openssl: PASS`
- `modules: PASS`

## Broader Runtime Suite

`winssl_runtime_suite_gh_26193849105_1.log` contains:

- `[WINSSL-RUNTIME] suite_start total=11`
- `[WINSSL-RUNTIME] compile_phase status=PASS total=11`
- `[WINSSL-RUNTIME] suite_summary passed=11 failed=0 total=11 success_rate=100`
- `[WINSSL-RUNTIME] suite_end status=PASS`

Observed WinSSL runtime lanes inside the broader suite include:

- callback surface summary:
  - `verify=pass`
  - `password=unsupported`
  - `info=pass`
- peer certificate surface:
  - `PASS`
- backend comparison:
  - `PASS`
- WinSSL session resumption truth:
  - `PASS`

## Session Runtime Truth

The downloaded runtime log keeps the current conservative WinSSL session-resumption truth intact:

- `public_reuse_truth=conservative_shared_path`
- `native_probe_truth=isolated_worker_opt_in`
- `observed_reuse=false`
- `session_configured=true`
- `native_probe_enabled=false`
- `native_probe_succeeded=false`
- `require_reuse=false`
- `require_native_reuse=false`

This means the fresh Windows proof confirms current published behavior, but does **not** justify upgrading WinSSL session resumption wording to “observed resumed-handshake success”.

## Follow-up Note

The run emitted one GitHub-hosted runner notice:

- `windows-latest requests are being redirected to windows-2025-vs2026 by June 15, 2026`

This is not a failure for the current head, but it is a future workflow-surface drift risk worth tracking in later CI/workflow truth audits.
