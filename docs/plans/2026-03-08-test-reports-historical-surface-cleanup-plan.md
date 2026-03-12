# 2026-03-08 Test-reports historical surface cleanup plan

## Goal

单独开一个历史面清理波次，处理已经积累在仓库里的 `test-reports/` 存量，降低 review 噪音。

这份计划只定义怎么做，不在本批次直接删历史文件。

## Why now

默认输出策略已经连续几波收口：新报告越来越少再落到 `test-reports/`。这意味着现在可以把“继续长新噪音”和“清理旧噪音”拆开处理：

- 默认路径治理：阻止新增
- 历史面清理：消化存量

## Non-goals

- 不改动当前 runtime / gate 语义
- 不把历史 evidence 直接硬删除到不可追踪
- 不在一个波次里同时做大规模脚本重构和历史归档

## Proposed phases

### Phase 1: Inventory the current surface

Run:

```bash
git ls-files -- test-reports
git status --short -- test-reports
find test-reports -maxdepth 2 -type f | sort
```

输出目标：

- tracked 历史文件清单
- untracked/modified 历史文件清单
- 可以按家族分桶的粗粒度 inventory

### Phase 2: Bucket by retention policy

建议至少拆成三桶：

1. `historical-reference`
   - 仍被活动文档引用
   - 需要保留，但应迁移到 `docs/archive/reports/` 或类似归档位
2. `generated-replayable`
   - 可以通过脚本重新生成
   - 应从跟踪面移除，仅保留脚本与合同
3. `obsolete-noise`
   - 已无活动引用、无治理价值
   - 应直接移出仓库跟踪面

### Phase 3: Create a migration manifest

建议新增一个 manifest，例如：

- `docs/archive/reports/2026-03-test-reports-migration-manifest.md`

内容包括：

- 来源路径
- 目标路径 / 删除决策
- 保留理由
- 是否仍被活动文档引用
- 是否可由脚本重建

### Phase 4: Land the cleanup in small batches

每批按一个家族落地，例如：

- Wave C 历史 bundle 报告
- Wave B / TLS13 历史 gate 报告
- 旧 smoke / ad-hoc 诊断报告

每批都需要：

- 一个 focused cleanup plan
- 一个引用完整性合同（如果有活动 docs 仍引用）
- 一个 `git status` / `git ls-files` 证据记录

## Guardrails

- 先迁移/删历史文件，再补引用修正，不要让 docs 悬空
- 没有 manifest 的大批量删除不落地
- 每次只做一类报告家族，保持 diff 可审查
- 历史归档目录必须和 active docs 隔离，避免“当前真相”继续被稀释

## Suggested first batch

优先级建议：

1. 清掉与当前 Wave C / Wave B 新默认目录已脱钩的旧 `test-reports` 家族
2. 保留一份活跃索引，把仍需引用的历史 evidence 迁到 `docs/archive/reports/`
3. 对可重放生成的报告，只保留脚本与合同，不再保留产物本身

## Success criteria

- `git ls-files -- test-reports` 显著下降
- `git status` 中 `test_reports_drift` 明显下降
- 活动文档不再直接依赖 `test-reports/` 历史路径
- 月度汇总继续只保留当前入口，而不是历史文件堆叠
