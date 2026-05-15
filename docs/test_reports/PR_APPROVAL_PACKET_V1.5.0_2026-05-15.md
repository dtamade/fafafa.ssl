# PR Approval Packet v1.5.0

Date: 2026-05-15

Status: `REQUEST_MERGE_APPROVAL`

## Summary

This PR requests merge approval for the already-pushed `v1.5.0` release-prep branch. The purpose of this batch is to externalize the current release-control truth into a reviewable GitHub PR without creating the `v1.5.0` tag and without publishing a GitHub Release.

## PR Intent

- base branch: `master`
- head branch: `release/v1.5.0-prep-2026-05-15`
- requested action: merge approval only
- explicitly not included:
  - `v1.5.0` tag creation
  - GitHub Release publication
  - Windows/WinSSL runtime evidence closeout

## Branch Truth

| Item | Value |
| --- | --- |
| Current release-prep branch | `release/v1.5.0-prep-2026-05-15` |
| Remote tracking branch | `origin/release/v1.5.0-prep-2026-05-15` |
| Current execution control plane | `release-control / v1.5.0 formalization` |
| Current release plan | `docs/plans/2026-05-12-release-v1.5.0-formalization.md` |
| Current readiness report | `docs/test_reports/RELEASE_READINESS_V1.5.0.md` |
| Current release-prep handoff | `docs/test_reports/RELEASE_PREP_HANDOFF_V1.5.0_2026-05-15.md` |

## Readiness Truth

- `docs/test_reports/RELEASE_READINESS_V1.5.0.md` is currently `READY_FOR_MAIN_MERGE`.
- In this repository, that readiness wording should be read as “ready to merge back to the default mainline branch,” and the actual default mainline branch is `master`.
- `docs/ROADMAP.md` currently remains `CLOSED_OUT_PENDING_APPROVAL`, so the remaining step is approval flow, not a new local release-control audit.

## Verification Summary

Current verified release-control evidence already attached to this branch:

- `tests/scripts/test_release_control_entrypoint_convergence_contract.sh` - PASS
- `tests/scripts/test_active_roadmap_references_contract.sh` - PASS
- `tests/scripts/test_platform_support_guidance_convergence_contract.sh` - PASS
- `tests/scripts/test_active_docs_historical_reference_labels_contract.sh` - PASS
- `tests/scripts/test_release_workflow_v1_5_0_contract.sh` - PASS
- `python3 scripts/compile_all_modules.py` - PASS (`185/185`, `0 failed`)
- `bash scripts/run_minimal_ci_gate.sh --fast-local` - PASS
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id release_prep_20260515` - PASS
- `python3 scripts/check_code_style.py src` - PASS
- `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local` - PASS
- `git diff --check` - PASS

## Boundary

- This PR does not merge automatically.
- This PR does not create the `v1.5.0` tag.
- This PR does not publish a GitHub Release.
- Windows/WinSSL runtime evidence remains explicitly deferred and static-only in this batch.
- If additional Windows runtime proof is later required, it should be handled as a separate follow-up path through the existing `wave-b-b2-manual.yml` workflow and evidence checklist.

## Reviewer Checklist

- confirm the PR targets `master`
- confirm the head branch is `release/v1.5.0-prep-2026-05-15`
- confirm release-control and readiness documents still agree
- confirm no production-code batch was mixed into this approval-only batch
- confirm `no-tag / no-release / Windows deferred` boundaries remain explicit

## After Merge

Not part of this batch:

1. explicit approval to create tag `v1.5.0`
2. explicit approval to run/publish GitHub Release
3. any separate Windows runtime evidence recovery if release policy later requires it
