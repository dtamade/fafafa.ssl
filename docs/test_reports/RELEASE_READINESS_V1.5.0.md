# Release Readiness v1.5.0

Date: 2026-05-12

Status: `BLOCKED_GITHUB_ACTIONS_BILLING`

## Summary

The Linux-side release preparation for `v1.5.0` is green after refreshing expired fixtures, aligning versioned documentation, enabling the release workflow, and clearing the strict style gate. The release must not be tagged or published yet because real Windows-host `WinSSL` runtime artifacts have not been collected.

The Wave B/B2 workflow was pushed and dispatched on GitHub Actions, but the run failed before any platform runner started because the account is blocked by billing or spending-limit settings. This is an external CI blocker, not a `WinSSL` implementation failure.

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

Current status: `BLOCKED_EXTERNAL_CI_BILLING`.

Required evidence before tag approval:

- Linux Wave B/B2 artifact.
- macOS Wave B/B2 artifact.
- Windows Wave B/B2 artifact.
- Windows quick smoke log.
- Wave B Windows gate summary and step logs.
- Broader WinSSL runtime suite transcript.

The local repository has the workflow and validation-bundle contracts needed to collect this evidence, but this Linux host does not itself prove Windows Schannel runtime behavior.

## GitHub Actions Attempt

| Item | Result |
| --- | --- |
| Workflow | `.github/workflows/wave-b-b2-manual.yml` |
| Ref | `glm51` |
| Head commit | `8491b914f5dd45604039a700935f90b5037eedde` |
| Run ID | `25698425400` |
| Run URL | `https://github.com/dtamade/fafafa.ssl/actions/runs/25698425400` |
| Inputs | `run_linux_baseline=true`, `strict_closure=true`, `run_id=release_1_5_0_20260512` |
| Result | `failure` before platform jobs started |
| Jobs | `setup` and `summary` failed; `linux-gate`, `macos-gate`, and `windows-gate` skipped |
| Artifact download | `no valid artifacts found to download` |
| Failure annotation | `The job was not started because recent account payments have failed or your spending limit needs to be increased.` |
| Classification | external GitHub Actions billing/spending-limit blocker |

Next action after the external blocker is cleared: rerun the same workflow on `glm51` with the same inputs, then review Linux, macOS, and Windows artifacts before changing this readiness status.

## High-Risk Conclusion

`WinSSL` remains the high-risk release area until real Windows-host artifacts are attached. Do not reopen `src/fafafa.ssl.winssl.*` unless the GitHub Windows lane returns a behavior failure rather than an environment or entrypoint failure.

## Tag Gate

Tag status: `BLOCKED`.

Do not create `v1.5.0` until:

- GitHub Actions billing/spending-limit access is restored or an equivalent trusted Windows host is used.
- Wave B/B2 Windows runtime proof is collected and reviewed.
- This readiness report is updated to `PASS_PENDING_APPROVAL`.
- The user explicitly approves tag creation.
