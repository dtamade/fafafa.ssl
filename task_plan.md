# Task Plan - v1.5.0 Release-Prep Push

## Goal

把当前已经通过静态审查与 release-control 契约验证的 `fafafa.ssl` 状态，收口成一个可推送到 `origin` 的 `v1.5.0` 预备分支，而不是继续停留在本地审查循环里。

## Current Status

- [completed] 从 `master` HEAD 创建 `release/v1.5.0-prep-2026-05-15`
- [completed] 写入 release-prep 计划、handoff 文档草稿与根 working-memory
- [completed] 在 release-prep 分支上重跑完整 release-control gates
- [in_progress] review、提交本批 metadata 更新并推送到 `origin`

## Current Blocker

- 当前没有产品实现 blocker；唯一剩余动作是把已 green 的本地状态变成远端可消费的 release-prep 分支。

## Current Queue

1. 更新 `task_plan.md` / `findings.md` / `progress.md`，把目标切到 release-prep 推送收口。
2. 新增 `docs/plans/2026-05-15-v1.5.0-release-prep-push.md` 与 `docs/test_reports/RELEASE_PREP_HANDOFF_V1.5.0_2026-05-15.md`。
3. 给出简短 review 结论，提交本批 metadata 变更。
4. 推送 `release/v1.5.0-prep-2026-05-15` 到 `origin`，记录最终 handoff 状态。

## Decision Locks

- 承载分支：`release/v1.5.0-prep-2026-05-15`
- 推送边界：只推送 release-prep 分支，不改 `origin/master`
- 发布边界：不创建 `v1.5.0` tag，不发 GitHub Release
- 默认入口：`docs/ROADMAP.md` -> `docs/plans/2026-05-12-release-v1.5.0-formalization.md` -> `docs/test_reports/RELEASE_READINESS_V1.5.0.md` -> `.github/README.md`
- 默认 gates：`compile_all_modules.py` -> `run_minimal_ci_gate.sh --fast-local` -> `run_freepascal_tls13_completeness_gate.sh --fast-local` -> `check_code_style.py src` -> `run_phase2_performance_baseline.sh --dry-run --fast-local`
- Windows/WinSSL：继续保持 `deferred / static-only follow-up` 身份，不在本批伪装成已完成的运行时证据

## Stop Condition

- `tests/scripts/test_release_control_entrypoint_convergence_contract.sh` 通过
- `tests/scripts/test_active_roadmap_references_contract.sh` 通过
- `tests/scripts/test_platform_support_guidance_convergence_contract.sh` 通过
- `tests/scripts/test_active_docs_historical_reference_labels_contract.sh` 通过
- `tests/scripts/test_release_workflow_v1_5_0_contract.sh` 通过
- `python3 scripts/compile_all_modules.py` 通过
- `bash scripts/run_minimal_ci_gate.sh --fast-local` 通过
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id release_prep_20260515` 通过
- `python3 scripts/check_code_style.py src` 通过
- `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local` 通过
- `git diff --check` 通过
- metadata commit 已创建并推送到 `origin/release/v1.5.0-prep-2026-05-15`
