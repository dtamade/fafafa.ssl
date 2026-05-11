# Release Readiness v1.5.0

Date: 2026-05-12

Status: `READY_FOR_MAIN_MERGE`

## Summary

The Linux-side release preparation for `v1.5.0` is green after refreshing expired fixtures, aligning versioned documentation, and clearing the strict style gate. A static Pascal audit also passes, so the release is ready to merge back to `main`.

Windows-host `WinSSL` runtime artifacts are deferred for this closeout because the current release scope is Linux-only and the remote quota is unavailable. That deferred evidence is not a blocker for the main merge.

## Version Truth

| Item | Result |
| --- | --- |
| Latest existing tag | `v1.4.3` |
| Source version | `FAFAFA_SSL_VERSION_STRING = '1.5.0'` |
| Interface version | `FAFAFA_SSL_INTERFACE_VERSION = 10500` |
| Lazarus package version | `1.5.0` |
| Changelog | `[1.5.0] - 2026-05-12` |
| README current version | `v1.5.0` |

## Local Release Gates

| Gate | Result | Evidence |
| --- | --- | --- |
| Compile all modules | PASS | `185/185`, `0 failed` |
| Minimal CI gate | PASS | compile `185/185`, module tests `17/17`, Phase 2 dry-run exercised |
| FreePascal TLS 1.3 completeness | PASS | `17 passed / 0 failed`, `tmp/test-reports/freepascal_tls13_completeness_release_1_5_0_20260512.md` |
| Code style | PASS | first strict run found 369 odd-indent errors; fixed 44 files / 369 lines mechanically; rerun passed |
| Phase 2 performance baseline dry-run | PASS | dry-run command generated successfully |

## Contract Gates

| Contract | Result |
| --- | --- |
| `tests/scripts/test_release_workflow_v1_5_0_contract.sh` | PASS |
| `tests/scripts/test_v1_5_0_static_pascal_audit_contract.sh` | PASS |
| `tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh` | PASS |
| `tests/scripts/test_winssl_windows_validation_bundle_contract.sh` | PASS |
| `tests/scripts/test_active_roadmap_references_contract.sh` | PASS |

## Static Audit

Current status: `PASS`.

The static audit confirms:

- the public facade still re-exports the expected Pascal entrypoints and interfaces
- `src/fafafa.ssl.factory.pas` still exposes the expected factory and helper APIs
- the active `src/fafafa.ssl*.pas` tree has no unresolved `TODO`, `FIXME`, `skeleton`, or `placeholder` markers
- the two WinSSL skeleton harnesses remain explicitly Windows-only and are outside the Linux release path

## Merge Gate

Main merge status: `READY`.

Tag creation remains deferred until the user explicitly approves it.
