# Historical Wave C pre-ci / submission / approval bucket cleanup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 清理历史 Wave C pre-ci / submission / approval 子桶：将仍被活动文档引用、以及为保持 archive 可导航必须保留的审批链证据迁到归档区，其余 replayable 结果从 `test-reports/` 跟踪面移除。

**Architecture:** 这一波只处理 `wave_c_b137_pre_ci_reenable_packet*`、`wave_c_b138_pre_ci_reenable_full_gate*`、`wave_c_b146_ci_reenable_submission_pack*`、`wave_c_b147_submission_pack_check*`、`wave_c_b148_ci_reenable_approval_brief*`、`wave_c_b149_ci_reenable_submission_bundle*`。外部文档具体引用 6 份；再加上为避免 archive 内断链需要保留的 3 份依赖证据，总计 9 份迁到 `docs/archive/reports/wave-c-pre-ci-submission-history/`。其余 7 份无当前引用、也不在保留闭包中的 replayable 结果直接删除。继续沿用月度 migration manifest，追加 Batch 6 记录移动与删除决策。

**Tech Stack:** Git file moves/deletions、Markdown manifest、Bash repo-hygiene contract

---

## Scope
- Historical files:
  - `test-reports/wave_c_b137_pre_ci_reenable_packet*`
  - `test-reports/wave_c_b138_pre_ci_reenable_full_gate*`
  - `test-reports/wave_c_b146_ci_reenable_submission_pack*`
  - `test-reports/wave_c_b147_submission_pack_check*`
  - `test-reports/wave_c_b148_ci_reenable_approval_brief*`
  - `test-reports/wave_c_b149_ci_reenable_submission_bundle*`
- Archive outputs:
  - `docs/archive/reports/wave-c-pre-ci-submission-history/`
  - `docs/archive/reports/2026-03-test-reports-migration-manifest.md`
- Contracts:
  - `tests/scripts/test_repo_hygiene_historical_wave_c_pre_ci_submission_bucket_contract.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Working memory:
  - `docs/plans/2026-03-current-summary.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Inventory
- Current tracked pre-ci / submission files: 16
- Concrete files referenced by docs: 6
- Additional dependency files kept to avoid archive broken-links: 3
- Total kept + archived: 9
- Unreferenced replayable files removed: 7
  - `wave_c_b137_pre_ci_reenable_packet_20260209_042505.md`
  - `wave_c_b137_pre_ci_reenable_packet_20260209_050311.md`
  - `wave_c_b138_pre_ci_reenable_full_gate_20260209_050311.md`
  - `wave_c_b146_ci_reenable_submission_pack_20260209_052849.md`
  - `wave_c_b147_submission_pack_check_20260209_052849.md`
  - `wave_c_b148_ci_reenable_approval_brief_20260209_052849.md`
  - `wave_c_b149_ci_reenable_submission_bundle_20260209_052849.md`

## RED -> GREEN Plan
1. 新增 `tests/scripts/test_repo_hygiene_historical_wave_c_pre_ci_submission_bucket_contract.sh`
2. 运行新合同，确认 RED
3. 创建 `docs/archive/reports/wave-c-pre-ci-submission-history/`
4. 迁移 9 份 retained pre-ci / submission / approval 历史证据到 archive，并把 docs 与 archive 内具体引用改到 archive 路径
5. 删除 7 份无引用且不在保留闭包中的 replayable 结果
6. 更新月度 manifest、summary 与 working memory
7. 接入 repo-hygiene batch / coverage contract 并运行 focused 验证
