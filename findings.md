# Findings - v1.5.0 Direct Merge

## 2026-05-15

- 用户已经明确选择“不走 PR”，所以当前正确路线变成 direct merge：
  - PR 不是技术必需，只是审批入口
  - 在当前仓库状态下，release-prep 分支上的代码与文档真相已经成立
  - 所以关闭 `#13` 并直接合并回 `master` 是更短路径

- PR `#13` 已经关闭且不再是当前路线：
  - PR: `#13`
  - URL: `https://github.com/dtamade/fafafa.ssl/pull/13`
  - state: `CLOSED`
  - 关闭原因：superseded by direct merge route

- direct merge 不会消除外部 GitHub Actions blocker：
  - PR checks 先前没有启动，是因为账户付款/额度问题
  - 即使改成直接 push 到 `master`，push-triggered workflows 仍然可能因为同一个外部问题而无法启动
  - 所以当前要区分“交付路径切换”与“GitHub Actions 账户恢复”这两件事

- 本地 `master` 仍然是 release-prep 的真实承载底座：
  - 当前本地 `master` 在 merge 前停在 `5f23652`
  - 它本来就领先 `origin/master` 94 个提交
  - `release/v1.5.0-prep-2026-05-15` 只是额外多出 direct-merge 之前的两批 PR 文档元数据
  - 所以 direct merge 的语义是“把 release-prep 的最终文档收口带回本地主线”，不是把一条陌生分支强行接进来

- release-prep 分支已经是稳定外显点，但目前远端还没有 PR：
  - `release/v1.5.0-prep-2026-05-15` 已经存在并跟踪 `origin/release/v1.5.0-prep-2026-05-15`
  - `gh pr list --head release/v1.5.0-prep-2026-05-15 --state all` 返回空数组
  - 这说明现在的实际缺口不是代码修复，而是 merge approval 入口还没建立

- 仓库没有现成 PR 模板：
  - `.github/pull_request_template.md` 不存在
  - `.github/PULL_REQUEST_TEMPLATE/` 目录也不存在
  - 所以本批需要在仓库内新增一个 approval packet 作为单一 PR 正文真相源

- GitHub branch protection 无法通过当前 API 自动发现：
  - `gh api repos/dtamade/fafafa.ssl/branches/master/protection` 返回 `403`
  - 返回信息是需要 GitHub Pro 或 public repo
  - 所以这批不能把“自动探测保护规则”当成执行前提，应该改用显式 checklist 和人工审批边界

- `RELEASE_READINESS_V1.5.0.md` 的 `READY_FOR_MAIN_MERGE` 属于旧措辞，不等于要改目标分支：
  - 当前仓库默认分支仍然是 `master`
  - 这个状态在本批应解释为“ready to merge back to the default mainline branch”
  - 为避免触碰已有 contract wording，本批在 approval packet 中解释映射关系，而不去重写 readiness 报告

- `mcp__ace_tool__.search_context` 当前不可用：
  - 返回 `ACE_TOKEN` 失效/无效
  - 这不是仓库事实问题，而是环境级检索工具失效
  - 本批继续依赖现有文档、shell 读取和 git/GitHub truth 完成，不等待该工具恢复

- merge-approval PR 已经创建出来：
  - PR: `#13`
  - URL: `https://github.com/dtamade/fafafa.ssl/pull/13`
  - state: `OPEN`
  - `mergeStateStatus`: `UNSTABLE`

- 当前 `UNSTABLE` 不是代码或 workflow 文本回归：
  - `gh pr checks 13` 显示 `Minimal Gate (Linux)`、`FreePascal TLS 1.3 Completeness`、`Code Quality (Light)`、`tls13-signer-gate` 全都没有真正启动
  - 四个 job 的共同 annotation 都是“recent account payments have failed or your spending limit needs to be increased”
  - 所以当前 PR 自动检查失败属于 GitHub Actions 账户计费/额度外部阻塞，不是这批分支内容把 CI 跑坏了

- `gh pr view` 的稳妥取数方式应记录下来：
  - 直接 `gh pr view release/v1.5.0-prep-2026-05-15 --json ...` 返回 `no pull requests found for branch`
  - 改用 PR 编号 `gh pr view 13 --json ...` 成功
  - 后续继续自动化时，优先记录 PR 编号再查询，避免 branch-name lookup 漂移

- `gh pr edit` 在当前仓库环境下也不可靠：
  - `gh pr edit 13 --body-file ...` 返回 classic Projects GraphQL 字段报错
  - 改用 `gh api repos/dtamade/fafafa.ssl/pulls/13 --method PATCH` 成功更新 title/body
  - 所以后续若还要脚本化刷新 PR 正文，优先走 REST API 而不是 `gh pr edit`

- 当前项目的“停滞感”主要不是来自新的代码故障，而是来自交付边界没有外显：
  - `docs/ROADMAP.md` 已经锁定 `release-control / v1.5.0 formalization`
  - `docs/test_reports/RELEASE_READINESS_V1.5.0.md` 已经写明 `READY_FOR_MAIN_MERGE`
  - 但本地 `master` 仍比 `origin/master` 超前 94 个提交
  - 这意味着大量真实进展还没有形成一个对外可消费的 release-prep 分支

- 继续“深审查 + 文档修边角”边际收益已经很低：
  - 最近高价值修复已经覆盖 focused gate / Wave C orchestration shell hardening / control-plane drift
  - 如果继续不设边界地审查，只会把 release-control 路线重新拖回本地循环

- 这批的正确目标不是再判断“能不能发”，而是把已经成立的收口事实外显：
  - 建一个独立的 `release/v1.5.0-prep-2026-05-15` 分支
  - 重跑当前 release-control gates
  - 推送一个明确的 handoff 分支
  - 继续保留 `no-merge / no-tag` 边界

- `glm51` 不适合作为这次收口载体：
  - 它代表旧的 Linux-only release closeout 轨迹
  - 继续复用它会把这批新的 release-control / control-plane 重构上下文重新混在旧分支语义里

- Windows/WinSSL 现在仍应保持 `deferred`：
  - 本地 Linux-side static closeout 已经成立
  - 但没有新的 Windows runtime 真证据
  - 所以这批只能把它作为 follow-up truth 写清楚，不能冒充成已完成项

- release-prep 路线已经通过完整本地收口复核：
  - release-control 契约全绿
  - `compile_all_modules.py` 185/185 通过
  - `run_minimal_ci_gate.sh --fast-local` 通过
  - `run_freepascal_tls13_completeness_gate.sh --fast-local --run-id release_prep_20260515` 通过
  - `check_code_style.py src` 通过
  - `run_phase2_performance_baseline.sh --dry-run --fast-local` 通过
  - 所以当前真正剩下的不是“继续判断项目是否健康”，而是把这份健康状态推送出去

- 这一步已经完成：
  - branch: `release/v1.5.0-prep-2026-05-15`
  - tracking: `origin/release/v1.5.0-prep-2026-05-15`
  - 当前仓库已经有一个明确的远端 handoff 点，可用于后续 merge / tag 审批，不必再依赖本地 `master ahead 94` 这个模糊状态
