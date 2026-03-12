# Historical test_p2 bucket cleanup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 清理历史 `test_p2_*` 结果桶：将仍被活动文档引用的少数 P2 结果迁到归档区，其余从 `test-reports/` 跟踪面移除。

**Architecture:** 这波只处理 `test-reports/test_p2_*` 家族。仍被文档引用的 3 份 comprehensive 结果迁到 `docs/archive/reports/test-p2-history/` 并更新引用；其余 15 份 replayable 结果从 `test-reports/` 跟踪面移除。沿用现有月度 migration manifest，追加一节记录来源、目标、引用和删除决策。

**Tech Stack:** Git file moves/deletions、Markdown manifest、Bash repo-hygiene contract

---

## Scope
- Historical files:
  - `test-reports/test_p2_*`
- Archive outputs:
  - `docs/archive/reports/test-p2-history/`
  - `docs/archive/reports/2026-03-test-reports-migration-manifest.md`
- Contracts:
  - `tests/scripts/test_repo_hygiene_historical_test_p2_bucket_contract.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Working memory:
  - `docs/plans/2026-03-current-summary.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Inventory
- Current tracked `test_p2_*`: 18
- Referenced by docs outside `test-reports/`: 3
- Referenced files:
  - `test_p2_store_comprehensive_result.txt`\n  - `test_p2_ocsp_comprehensive_result.txt`\n  - `test_p2_ts_comprehensive_result.txt`\n

## RED -> GREEN Plan
1. 新增 `tests/scripts/test_repo_hygiene_historical_test_p2_bucket_contract.sh`
2. 运行新合同，确认 RED
3. 创建 `docs/archive/reports/test-p2-history/`
4. 迁移 3 份仍被文档引用的历史结果到 archive 并更新 docs 引用
5. 删除剩余 15 份未被引用的 replayable `test_p2_*`
6. 更新月度 manifest、summary 与 working memory
7. 接入 repo-hygiene batch / coverage contract 并运行 focused 验证
