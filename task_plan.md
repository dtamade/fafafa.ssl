# Task Plan - Release Control Plane Realignment

## Goal

把仓库默认工程入口、workflow 说明和根计划文件统一收口到 `release-control / v1.5.0 formalization`，不再把 Wave C closeout 链当成当前默认执行控制面。

## Current Status

- [completed] contract RED observed on the old Wave C default-entry wording
- [completed] active docs realigned to the release-control chain
- [completed] workflow surface doc updated to expose `release.yml`
- [completed] root planning files reset to current-control-plane format
- [in_progress] final review and commit

## Current Blocker

- 无新的实现 blocker；当前只剩 final review、状态确认和提交收口。

## Current Queue

1. 复核本批差异，确认 active docs / workflow docs / root planning files 讲的是同一套控制面。
2. 提交本批 `release-control` 工作流修复。
3. 下一批继续深审时，优先沿当前 active release-control gates 与真实 workflow surface 深挖，不再从 Wave C 历史入口重新发散。

## Decision Locks

- 默认工程入口：`docs/ROADMAP.md` -> `docs/plans/2026-05-12-release-v1.5.0-formalization.md` -> `docs/test_reports/RELEASE_READINESS_V1.5.0.md` -> `.github/README.md`
- 默认命令链：`compile_all_modules.py` -> `run_minimal_ci_gate.sh --fast-local` -> `run_freepascal_tls13_completeness_gate.sh --fast-local` -> `check_code_style.py src` -> `run_phase2_performance_baseline.sh --dry-run --fast-local`
- Wave C 角色：`closeout / approval / historical reference only`
- Windows/WinSSL：保留显式 follow-up 身份，但不是当前 Linux-side static closeout 的默认前置条件

## Stop Condition

- `tests/scripts/test_release_control_entrypoint_convergence_contract.sh` 通过
- `tests/scripts/test_active_roadmap_references_contract.sh` 通过
- `tests/scripts/test_platform_support_guidance_convergence_contract.sh` 通过
- `tests/scripts/test_active_docs_historical_reference_labels_contract.sh` 通过
- `tests/scripts/test_release_workflow_v1_5_0_contract.sh` 通过
- `git diff --check` 通过
- 完成 review 结论并提交
