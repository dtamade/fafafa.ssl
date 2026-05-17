# Release Readiness v1.5.0

Date: 2026-05-17

Status: `PASS_PENDING_APPROVAL`

## Summary

The local release-control gates for `v1.5.0` are green, the static Pascal audit passes, and the current GitHub Actions runtime truth is also green on the same head. There is no active technical blocker left on the current release surface.

What remains is the explicit human approval gate for creating the `v1.5.0` tag and GitHub Release. The latest existing tag is still `v1.4.3`.

## Version Truth

| Item | Result |
| --- | --- |
| Latest existing tag | `v1.4.3` |
| Source version | `FAFAFA_SSL_VERSION_STRING = '1.5.0'` |
| Interface version | `FAFAFA_SSL_INTERFACE_VERSION = 10500` |
| Lazarus package version | `1.5.0` |
| Changelog | `[1.5.0] - 2026-05-12` |
| README current version | `v1.5.0` |

## GitHub Runtime Evidence

| Surface | Result | Evidence |
| --- | --- | --- |
| Manual cross-platform runtime workflow | PASS | `wave-b-b2-manual.yml` run `25989095571` on head `b95044d` completed with `windows-gate` / `macos-gate` / `linux-gate` / `summary` all `SUCCESS` |
| Default CI workflow | PASS | `CI` run `25989090032` on head `b95044d` completed `SUCCESS` |

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

## Approval Gate

Release-control status: `PASS_PENDING_APPROVAL`.

Do not create the `v1.5.0` tag or GitHub Release until the user explicitly approves it.
