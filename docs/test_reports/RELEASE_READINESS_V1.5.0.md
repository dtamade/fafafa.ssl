# Release Readiness v1.5.0

Date: 2026-05-12

Status: `BLOCKED_PENDING_WINDOWS_RUNTIME_PROOF`

## Summary

The Linux-side release preparation for `v1.5.0` is green after refreshing expired fixtures, aligning versioned documentation, enabling the release workflow, and clearing the strict style gate. The release must not be tagged or published yet because real Windows-host `WinSSL` runtime artifacts have not been collected in this batch.

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
| `tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh` | PASS |
| `tests/scripts/test_winssl_windows_validation_bundle_contract.sh` | PASS |
| `tests/scripts/test_active_roadmap_references_contract.sh` | PASS |

## WinSSL Runtime Proof

Current status: `PENDING`.

Required evidence before tag approval:

- Linux Wave B/B2 artifact.
- macOS Wave B/B2 artifact.
- Windows Wave B/B2 artifact.
- Windows quick smoke log.
- Wave B Windows gate summary and step logs.
- Broader WinSSL runtime suite transcript.

The local repository has the workflow and validation-bundle contracts needed to collect this evidence, but this Linux host does not itself prove Windows Schannel runtime behavior. The next action is to push the `glm51` branch and dispatch `.github/workflows/wave-b-b2-manual.yml` with `run_id=release_1_5_0_20260512`.

## High-Risk Conclusion

`WinSSL` remains the high-risk release area until real Windows-host artifacts are attached. Do not reopen `src/fafafa.ssl.winssl.*` unless the GitHub Windows lane returns a behavior failure rather than an environment or entrypoint failure.

## Tag Gate

Tag status: `BLOCKED`.

Do not create `v1.5.0` until:

- Wave B/B2 Windows runtime proof is collected and reviewed.
- This readiness report is updated to `PASS_PENDING_APPROVAL`.
- The user explicitly approves tag creation.
