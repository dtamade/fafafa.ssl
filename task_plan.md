# Task Plan - v1.5.0 Direct Merge

## Goal

把已经推送到远端的 `release/v1.5.0-prep-2026-05-15` 在不继续使用 GitHub PR 审批流的前提下，直接合并回本地 `master` 并推送到 `origin/master`，同时保留完整的决策与外部阻塞记录。

## Current Status

- [completed] `release/v1.5.0-prep-2026-05-15` 已推送到 `origin`
- [completed] PR `#13` 已关闭，不再作为当前交付路线
- [completed] 写入 direct-merge 计划、关闭说明与根 working-memory
- [completed] 跑 focused contract checks
- [pending] 提交 direct-merge metadata、合并并推送 `master`

## Current Blocker

- 当前没有本地代码 blocker；如果要求远端 workflow 变绿，主要外部 blocker 仍是 GitHub Actions 账户计费/额度问题。

## Current Queue

1. 新增 `docs/plans/2026-05-15-v1.5.0-direct-merge.md`。
2. 更新 `docs/test_reports/PR_APPROVAL_PACKET_V1.5.0_2026-05-15.md` 与根 working-memory，记录 PR `#13` 已关闭并转入 direct merge。
3. 跑 focused contract checks 与 `git diff --check`。
4. 在 `release/v1.5.0-prep-2026-05-15` 上提交 direct-merge metadata 批次并推送。
5. 切回 `master`，合并 `release/v1.5.0-prep-2026-05-15` 并 push 到 `origin/master`。

## Decision Locks

- 承载分支：`release/v1.5.0-prep-2026-05-15`
- 当前路线：关闭 PR，直接合并到 `master`
- 推送边界：先更新 release-prep 分支元数据，再直接更新 `origin/master`
- 发布边界：不创建 `v1.5.0` tag，不发 GitHub Release
- 默认入口：`docs/ROADMAP.md` -> `docs/plans/2026-05-12-release-v1.5.0-formalization.md` -> `docs/test_reports/RELEASE_READINESS_V1.5.0.md` -> `.github/README.md`
- 默认 focused gates：`test_release_control_entrypoint_convergence_contract.sh` -> `test_active_roadmap_references_contract.sh` -> `test_release_workflow_v1_5_0_contract.sh` -> `git diff --check`
- Windows/WinSSL：继续保持 `deferred / static-only follow-up` 身份，不在本批伪装成已完成的运行时证据
- 当前 PR 历史记录：`#13` 已关闭，作为 direct merge 前的历史审批尝试保留
- 当前 GitHub check 风险：即使 direct merge 到 `master`，push-triggered workflows 仍可能因账户计费/额度问题无法启动
- PR body 更新路径：`gh pr edit` 因 classic Projects GraphQL 字段报错不可用，历史刷新需改走 `gh api repos/.../pulls/13 --method PATCH`

## Stop Condition

- `tests/scripts/test_release_control_entrypoint_convergence_contract.sh` 通过
- `tests/scripts/test_active_roadmap_references_contract.sh` 通过
- `tests/scripts/test_release_workflow_v1_5_0_contract.sh` 通过
- `git diff --check` 通过
- PR `#13` 已关闭且未合并
- direct-merge metadata commit 已创建并推送到 `origin/release/v1.5.0-prep-2026-05-15`
- `master` merge commit 已创建并推送到 `origin/master`
- GitHub-side startup failure 已记录为外部 blocker，而不是本地 release-control 失败
