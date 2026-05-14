# Findings - Release Control Plane Realignment

## 2026-05-15

- 控制面漂移是真问题，不是文案偏好：
  - `docs/README.md`
  - `docs/DOCUMENTATION_INDEX.md`
  - `docs/PLATFORM_SUPPORT.md`
  仍把 `Wave C canonical chain` 当成默认工程入口，但当前 active roadmap、release workflow 和 release readiness 已经转到 `release-control / v1.5.0 formalization`。

- `.github/README.md` 漏掉了 active `release.yml`，导致 workflow surface 本身不完整；这会让操作者只看到日常 CI，看不到当前 release-control 的真实收口入口。

- 根计划文件失去控制面价值：
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
  长期堆叠历史 batch，已经不再服务“当前唯一目标 / 当前 blocker / 下一步怎么做”，这会直接拖慢继续执行和审查节奏。

- 这批收口后的 durable truth：
  - 默认工程入口是 `ROADMAP -> release formalization plan -> release readiness -> workflow surface`
  - 默认命令链是 compile / minimal gate / FreePascal TLS13 focused gate / style / Phase 2 dry-run
  - Wave C 只保留 `closeout / approval / historical reference` 身份
  - Windows/WinSSL follow-up 仍然显式存在，但不再伪装成当前 Linux-side static closeout 的默认本地前置条件

- 历史证据并没有丢：
  - 历史 batch 细节继续保留在 `docs/plans/`、`docs/test_reports/` 和 git 历史
  - 根计划文件这次是有意恢复为“当前控制面”，而不是继续充当永久档案堆栈

- 工具备注：
  - `ace-tool/search_context` 本轮因 `ACE_TOKEN` 无效不可用
  - 本批改动因此完全基于仓内文件真值和契约测试推进
