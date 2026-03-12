# Historical Wave C local-first / local-guard bucket cleanup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 清理历史 Wave C local-first / local-guard 子桶：将仍被活动文档引用、以及为保持 archive 可导航必须保留的 local-first/local-guard 证据迁到归档区，其余 replayable 结果从 `test-reports/` 跟踪面移除。

**Architecture:** 这一波只处理 `wave_c_b123_local_first_continuity*`、`wave_c_b124_local_drift_watch*`、`wave_c_b125_local_guard_bundle*`、`wave_c_b126_local_guard_history*`、`wave_c_b129_oncall_check*`、`wave_c_b132_local_first_status_snapshot*`、`wave_c_b139_local_guard_cleanup_plan*`、`wave_c_b140_local_guard_consistency*`、`wave_c_b142_local_guard_status*`、`wave_c_b143_alert_thresholds*`、`wave_c_b144_local_guard_ops_pack*`。外部文档具体引用 11 份；再加上为避免 archive 内断链需要保留的 45 份依赖证据，总计 56 份迁到 `docs/archive/reports/wave-c-local-first-guard-history/`。其余 23 份无当前引用、也不在保留闭包中的 replayable 结果直接删除。继续沿用月度 migration manifest，追加 Batch 7 记录移动与删除决策。

**Tech Stack:** Git file moves/deletions、Markdown manifest、Bash repo-hygiene contract

---

## Scope
- Historical files:
  - `test-reports/wave_c_b123_local_first_continuity*`
  - `test-reports/wave_c_b124_local_drift_watch*`
  - `test-reports/wave_c_b125_local_guard_bundle*`
  - `test-reports/wave_c_b126_local_guard_history*`
  - `test-reports/wave_c_b129_oncall_check*`
  - `test-reports/wave_c_b132_local_first_status_snapshot*`
  - `test-reports/wave_c_b139_local_guard_cleanup_plan*`
  - `test-reports/wave_c_b140_local_guard_consistency*`
  - `test-reports/wave_c_b142_local_guard_status*`
  - `test-reports/wave_c_b143_alert_thresholds*`
  - `test-reports/wave_c_b144_local_guard_ops_pack*`
- Archive outputs:
  - `docs/archive/reports/wave-c-local-first-guard-history/`
  - `docs/archive/reports/2026-03-test-reports-migration-manifest.md`
- Contracts:
  - `tests/scripts/test_repo_hygiene_historical_wave_c_local_first_guard_bucket_contract.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Working memory:
  - `docs/plans/2026-03-current-summary.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Inventory
- Current tracked local-first / local-guard files: 79
- Concrete files referenced by docs: 11
- Additional dependency files kept to avoid archive broken-links: 45
- Total kept + archived: 56
- Unreferenced replayable files removed: 23

## RED -> GREEN Plan
1. 新增 `tests/scripts/test_repo_hygiene_historical_wave_c_local_first_guard_bucket_contract.sh`
2. 运行新合同，确认 RED
3. 创建 `docs/archive/reports/wave-c-local-first-guard-history/`
4. 迁移 56 份 retained local-first / local-guard 历史证据到 archive，并把 docs 与 archive 内具体引用改到 archive 路径
5. 删除 23 份无引用且不在保留闭包中的 replayable 结果
6. 更新月度 manifest、summary 与 working memory
7. 接入 repo-hygiene batch / coverage contract 并运行 focused 验证
