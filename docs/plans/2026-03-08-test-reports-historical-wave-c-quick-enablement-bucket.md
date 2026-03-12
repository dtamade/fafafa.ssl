# Historical `wave_c` quick / enablement bucket cleanup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 清理历史 Wave C quick / enablement 证据桶：将仍被活动文档引用、以及为保持 archive 可导航而必须保留的 quick/enablement 证据迁到归档区，其余 replayable 结果从 `test-reports/` 跟踪面移除。

**Architecture:** 这一波只处理 quick / enablement 子族：`wave_c_quick_sprint_bundle*`、`wave_c_b101_validation*`、`wave_c_b107_threshold_eval*`、`wave_c_b108_default_on_readiness*`、`wave_c_b109_canary_rollout*`、`wave_c_b110_recheck*`、`wave_c_b110_rollback_drill*`、`wave_c_b115_workflow_enable_prereq*`、`wave_c_b119_first_run_preflight*`、`wave_c_b120_post_trigger_observability*`。外部文档具体引用 19 份；再加上为避免 archive 内断链所需的 7 份依赖证据，总计 26 份迁到 `docs/archive/reports/wave-c-quick-enablement-history/`。其余 4 份无当前引用、也不在保留闭包里的 replayable 结果直接从 tracked 历史面删除。继续沿用月度 migration manifest，追加 Batch 5 记录移动与删除决策。

**Tech Stack:** Git file moves/deletions、Markdown manifest、Bash repo-hygiene contract

---

## Scope
- Historical files:
  - `test-reports/wave_c_quick_sprint_bundle*`
  - `test-reports/wave_c_b101_validation*`
  - `test-reports/wave_c_b107_threshold_eval*`
  - `test-reports/wave_c_b108_default_on_readiness*`
  - `test-reports/wave_c_b109_canary_rollout*`
  - `test-reports/wave_c_b110_recheck*`
  - `test-reports/wave_c_b110_rollback_drill*`
  - `test-reports/wave_c_b115_workflow_enable_prereq*`
  - `test-reports/wave_c_b119_first_run_preflight*`
  - `test-reports/wave_c_b120_post_trigger_observability*`
- Archive outputs:
  - `docs/archive/reports/wave-c-quick-enablement-history/`
  - `docs/archive/reports/2026-03-test-reports-migration-manifest.md`
- Contracts:
  - `tests/scripts/test_repo_hygiene_historical_wave_c_quick_enablement_bucket_contract.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Working memory:
  - `docs/plans/2026-03-current-summary.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Inventory
- Current tracked quick / enablement files: 30
- Concrete files referenced by docs/plans: 19
- Additional dependency files kept to avoid archive broken-links: 7
- Total kept + archived: 26
- Unreferenced replayable files removed: 4
  - `wave_c_b101_validation_latest.md`
  - `wave_c_b107_threshold_eval_20260208_051900.md`
  - `wave_c_b115_workflow_enable_prereq_20260208_174600.md`
  - `wave_c_b115_workflow_enable_prereq_20260208_174600_strict.md`

## RED -> GREEN Plan
1. 新增 `tests/scripts/test_repo_hygiene_historical_wave_c_quick_enablement_bucket_contract.sh`
2. 运行新合同，确认 RED
3. 创建 `docs/archive/reports/wave-c-quick-enablement-history/`
4. 迁移 26 份 quick / enablement 历史证据到 archive，并把 docs 与 archive 内的具体引用改到 archive 路径
5. 删除 4 份无引用且不在保留闭包中的 replayable 结果
6. 更新月度 manifest、summary 与 working memory
7. 接入 repo-hygiene batch / coverage contract 并运行 focused 验证
