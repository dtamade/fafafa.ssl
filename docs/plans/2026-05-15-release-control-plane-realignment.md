# Release-Control Plane Realignment Plan

## Goal

修复当前仓库控制面漂移：把默认工程入口、workflow 说明和根计划文件统一收口到 `release-control / v1.5.0 formalization`，同时把 Wave C 明确降级为 closeout / approval / historical reference。

## Architecture

- `docs/ROADMAP.md` 继续作为稳定 truth source。
- `docs/plans/2026-05-12-release-v1.5.0-formalization.md` 是当前 release-control 执行计划。
- `docs/test_reports/RELEASE_READINESS_V1.5.0.md` 是当前 release-control 收口结论。
- `README.md`、`docs/README.md`、`docs/DOCUMENTATION_INDEX.md`、`docs/PLATFORM_SUPPORT.md`、`.github/README.md` 必须对外讲同一套入口。
- `task_plan.md` / `findings.md` / `progress.md` 必须回到“当前唯一目标 + 当前 blocker + 下一批次 + 已验证证据”的控制面职责，不再堆叠为历史批次流水账。

## Files

- Modify: `README.md`
- Modify: `docs/README.md`
- Modify: `docs/DOCUMENTATION_INDEX.md`
- Modify: `docs/PLATFORM_SUPPORT.md`
- Modify: `docs/ROADMAP.md`
- Modify: `.github/README.md`
- Modify: `docs/plans/2026-05-12-release-v1.5.0-formalization.md`
- Add: `docs/plans/2026-05-15-release-control-plane-realignment.md`
- Add/replace: `tests/scripts/test_release_control_entrypoint_convergence_contract.sh`
- Modify: `tests/scripts/test_active_roadmap_references_contract.sh`
- Modify: `tests/scripts/test_platform_support_guidance_convergence_contract.sh`
- Modify: `tests/scripts/test_active_docs_historical_reference_labels_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: Contract RED

Run:

```bash
bash -n tests/scripts/test_release_control_entrypoint_convergence_contract.sh
bash tests/scripts/test_release_control_entrypoint_convergence_contract.sh
bash tests/scripts/test_active_roadmap_references_contract.sh
bash tests/scripts/test_platform_support_guidance_convergence_contract.sh
bash tests/scripts/test_active_docs_historical_reference_labels_contract.sh
```

Expected:

- new release-control contract fails before docs are edited
- roadmap/platform/history-label contracts fail exactly on the old Wave C default-entry wording

## Task 2: Doc And Workflow Surface Realignment

Update the active docs so they all say:

- default control plane = `ROADMAP -> release formalization plan -> release readiness -> workflow surface`
- current commands = compile gate, minimal gate, FreePascal focused gate, style gate, Phase 2 dry-run
- Wave C role = closeout / approval / historical reference
- `.github/README.md` must expose `release.yml` as an active workflow

## Task 3: Root Planning File Reset

Rewrite:

- `task_plan.md` to show the current unique goal, blocker, next queue, stop condition
- `findings.md` to keep only current workflow findings plus durable conclusions
- `progress.md` to keep this batch’s RED/GREEN evidence and command log

## Task 4: Verification

Run:

```bash
bash tests/scripts/test_release_control_entrypoint_convergence_contract.sh
bash tests/scripts/test_active_roadmap_references_contract.sh
bash tests/scripts/test_platform_support_guidance_convergence_contract.sh
bash tests/scripts/test_active_docs_historical_reference_labels_contract.sh
bash tests/scripts/test_release_workflow_v1_5_0_contract.sh
git diff --check
```

Expected:

- active docs agree on the release-control entrypoint
- Wave C is no longer presented as the default engineering chain
- workflow docs expose the active release workflow
- root planning files become concise and executable again

## Task 5: Review And Commit

- write the final verification results into `progress.md`
- write the control-plane conclusions into `findings.md`
- give a short review conclusion
- create one commit for this batch
