# Task Plan - v1.5.0 PR Approval

## Goal

把已经推送到远端的 `release/v1.5.0-prep-2026-05-15` 收口成一个可审查、可合并的 GitHub PR，让当前 release-control 成果进入明确的 merge approval 流，而不是继续停留在“远端已有分支但没有审批入口”的状态。

## Current Status

- [completed] `release/v1.5.0-prep-2026-05-15` 已推送到 `origin`
- [completed] release-prep handoff 与 readiness 文档已经收口
- [completed] 写入 PR approval 计划、approval packet 与根 working-memory
- [completed] 跑 focused contract checks
- [pending] 创建 merge-approval PR、同步最终 PR 元数据

## Current Blocker

- 当前没有代码 blocker；当前唯一待外显的交付动作是 GitHub PR 审批入口。

## Current Queue

1. 新增 `docs/plans/2026-05-15-v1.5.0-pr-approval.md` 与 `docs/test_reports/PR_APPROVAL_PACKET_V1.5.0_2026-05-15.md`。
2. 更新 `task_plan.md` / `findings.md` / `progress.md`，把目标切到 PR 审批批次。
3. 跑 focused contract checks 与 `git diff --check`。
4. 创建或更新唯一的 merge-approval PR。
5. 如需继续，下一批再单独进入 merge 后 tag/release 路线。

## Decision Locks

- 承载分支：`release/v1.5.0-prep-2026-05-15`
- PR 基线：`master`
- 推送边界：只更新 release-prep 分支，不直接改 `origin/master`
- 发布边界：不创建 `v1.5.0` tag，不发 GitHub Release
- 默认入口：`docs/ROADMAP.md` -> `docs/plans/2026-05-12-release-v1.5.0-formalization.md` -> `docs/test_reports/RELEASE_READINESS_V1.5.0.md` -> `.github/README.md`
- PR 正文源：`docs/test_reports/PR_APPROVAL_PACKET_V1.5.0_2026-05-15.md`
- 默认 focused gates：`test_release_control_entrypoint_convergence_contract.sh` -> `test_active_roadmap_references_contract.sh` -> `test_release_workflow_v1_5_0_contract.sh` -> `git diff --check`
- Windows/WinSSL：继续保持 `deferred / static-only follow-up` 身份，不在本批伪装成已完成的运行时证据
- GitHub 约束：仓库没有现成 PR 模板；branch protection API 当前返回 `403`，所以本批使用 repo 内 approval packet + 明确 checklist 代替自动发现

## Stop Condition

- `tests/scripts/test_release_control_entrypoint_convergence_contract.sh` 通过
- `tests/scripts/test_active_roadmap_references_contract.sh` 通过
- `tests/scripts/test_release_workflow_v1_5_0_contract.sh` 通过
- `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local` 通过
- `git diff --check` 通过
- PR approval asset commit 已创建并推送到 `origin/release/v1.5.0-prep-2026-05-15`
- `gh pr view --json number,title,state,url,mergeStateStatus,reviewDecision,headRefName,baseRefName` 可返回唯一 PR 元数据
