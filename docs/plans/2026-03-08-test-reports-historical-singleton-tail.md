# Historical test-reports singleton tail cleanup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 清理历史 `test-reports/` 跟踪面的最后 3 个 singleton 尾项，完成 tracked historical surface 清零。

**Architecture:** 这一波只处理三个遗留固定名产物：`test-reports/test_provider_result.txt`、`test-reports/test_p4_engine_result.txt`、`test-reports/mbedtls_test_suite_20260209.md`。其中 `test_provider` / `test_p4_engine` 已由 `scripts/run_all_module_tests.sh` 切换为按 `RUN_ID` 命名的结果文件；`mbedtls_test_suite_20260209.md` 对应的能力状态已经由 `tests/test_mbedtls_framework.pas` 与 `docs/test_reports/MBEDTLS_BACKEND_STATUS_REPORT.md` 覆盖，因此这三份都视为 replayable historical evidence，不再迁 archive，只在月度 migration manifest 记录删除决策，并加一个 focused contract 锁定 `git ls-files -- test-reports` 为 `0`。

**Tech Stack:** Git deletions、Markdown manifest、Bash repo-hygiene contract

---

## Scope
- Historical files:
  - `test-reports/test_provider_result.txt`
  - `test-reports/test_p4_engine_result.txt`
  - `test-reports/mbedtls_test_suite_20260209.md`
- Manifest / summary:
  - `docs/archive/reports/2026-03-test-reports-migration-manifest.md`
  - `docs/plans/2026-03-current-summary.md`
- Contracts:
  - `tests/scripts/test_repo_hygiene_historical_singleton_tail_contract.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Working memory:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Inventory
- Remaining tracked singleton files: 3
- Concrete active-doc references outside cleanup planning: 0
- Replayable sources:
  - `scripts/run_all_module_tests.sh` → `test_provider_<runid>_result.txt` / `test_p4_engine_<runid>_result.txt`
  - `tests/test_mbedtls_framework.pas` + `docs/test_reports/MBEDTLS_BACKEND_STATUS_REPORT.md` → MbedTLS suite status

## RED -> GREEN Plan
1. 新增 `tests/scripts/test_repo_hygiene_historical_singleton_tail_contract.sh`
2. 运行新合同，确认 RED
3. 删除 3 份遗留 singleton 产物
4. 更新 migration manifest、March summary 与 working memory
5. 接入 repo-hygiene batch / coverage contract
6. 运行 focused + batch + diff 验证，确认 tracked `test-reports/` 为 `0`
