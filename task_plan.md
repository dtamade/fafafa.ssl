# Task Plan - v1.5.0 PR Approval

## Goal

把已经推送到远端的 `release/v1.5.0-prep-2026-05-15` 收口成一个可审查、可合并的 GitHub PR，让当前 release-control 成果进入明确的 merge approval 流，而不是继续停留在“远端已有分支但没有审批入口”的状态。

## Current Status

- [completed] `release/v1.5.0-prep-2026-05-15` 已推送到 `origin`
- [completed] release-prep handoff 与 readiness 文档已经收口
- [completed] 写入 PR approval 计划、approval packet 与根 working-memory
- [completed] 跑 focused contract checks
- [completed] 创建 merge-approval PR
- [completed] 同步最终 PR 元数据与 GitHub-side blocker

## Current Blocker

- 当前本地执行已收口；后续如果要求 GitHub checks 变绿，主要外部 blocker 是 GitHub Actions 账户计费/额度问题导致 PR checks 未启动。

## Current Queue

1. 新增 `docs/plans/2026-05-15-v1.5.0-pr-approval.md` 与 `docs/test_reports/PR_APPROVAL_PACKET_V1.5.0_2026-05-15.md`。
2. 更新 `task_plan.md` / `findings.md` / `progress.md`，把目标切到 PR 审批批次。
3. 跑 focused contract checks 与 `git diff --check`。
1. 如需继续，先处理 GitHub Actions 账户状态，使 PR checks 能正常启动。
2. 或在人工审批接受当前外部阻塞说明的前提下，进入 merge 决策。
3. merge 后另起一批进入 tag/release 路线。

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
- 当前 PR：`#13` -> `https://github.com/dtamade/fafafa.ssl/pull/13`
- 当前 GitHub check 状态：`UNSTABLE`，原因是工作流因账户计费/额度问题未启动，不是分支代码失败
- PR body 更新路径：`gh pr edit` 因 classic Projects GraphQL 字段报错不可用，需改走 `gh api repos/.../pulls/13 --method PATCH`

## Stop Condition

- `tests/scripts/test_release_control_entrypoint_convergence_contract.sh` 通过
- `tests/scripts/test_active_roadmap_references_contract.sh` 通过
- `tests/scripts/test_release_workflow_v1_5_0_contract.sh` 通过
- `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local` 通过
- `git diff --check` 通过
- PR approval asset commit 已创建并推送到 `origin/release/v1.5.0-prep-2026-05-15`
- `gh pr view --json number,title,state,url,mergeStateStatus,reviewDecision,headRefName,baseRefName` 可返回唯一 PR 元数据
- GitHub-side startup failure 已记录为外部 blocker，而不是本地 release-control 失败
