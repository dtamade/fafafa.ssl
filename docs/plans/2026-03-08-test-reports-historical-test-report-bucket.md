# Historical test-report bucket cleanup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 清理历史 `test_report_*.txt` 跟踪面：将仍被文档引用的少数证据迁到归档区，其余从 `test-reports/` 跟踪面移除。

**Architecture:** 这波只处理 `scripts/run_all_module_tests.sh` 生成的历史 `test_report_*.txt` 家族。保留引用的文件迁到 `docs/archive/reports/test-report-history/` 并更新引用；未被仓库文档引用的同家族文件直接从 `test-reports/` 跟踪面移除。再补一个 focused repo-hygiene 合同，防止该家族重新以 tracked historical bucket 的形式回流。

**Tech Stack:** Git file moves/deletions、Markdown manifest、Bash repo-hygiene contract

---

## Scope
- Historical files:
  - `test-reports/test_report_*.txt`
- Archive outputs:
  - `docs/archive/reports/test-report-history/`
  - `docs/archive/reports/2026-03-test-reports-migration-manifest.md`
- Contracts:
  - `tests/scripts/test_repo_hygiene_historical_test_report_bucket_contract.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Working memory:
  - `docs/plans/2026-03-current-summary.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Inventory
- Current tracked `test_report_*.txt`: 106
- Referenced by docs outside `test-reports/`: 28
- Generator source: `scripts/run_all_module_tests.sh` writes `test_report_$RUN_ID.txt`

## RED -> GREEN Plan
1. 新增 `tests/scripts/test_repo_hygiene_historical_test_report_bucket_contract.sh`
2. 运行新合同，确认 RED
3. 创建 migration manifest 与 archive 目录
4. 迁移 28 份仍被文档引用的历史证据到 `docs/archive/reports/test-report-history/`
5. 删除剩余未被引用的 `test_report_*.txt`
6. 更新 docs 引用与月度/working memory
7. 接入 repo-hygiene batch / coverage contract 并运行 focused 验证
