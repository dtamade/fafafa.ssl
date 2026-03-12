# Historical wave_b bucket cleanup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 清理历史 `wave_b_*` 结果桶：将仍被活动文档引用的 Wave B 证据迁到归档区，其余从 `test-reports/` 跟踪面移除。

**Architecture:** 这波只处理 `test-reports/wave_b_*` 家族。仍被文档点名的 11 份具体 Wave B 证据迁到 `docs/archive/reports/wave-b-history/` 并更新引用；其余 13 份 replayable 结果从 `test-reports/` 跟踪面移除。继续沿用月度 migration manifest，追加 Batch 3 记录来源、目标、引用和删除决策。

**Tech Stack:** Git file moves/deletions、Markdown manifest、Bash repo-hygiene contract

---

## Scope
- Historical files:
  - `test-reports/wave_b_*`
- Archive outputs:
  - `docs/archive/reports/wave-b-history/`
  - `docs/archive/reports/2026-03-test-reports-migration-manifest.md`
- Contracts:
  - `tests/scripts/test_repo_hygiene_historical_wave_b_bucket_contract.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Working memory:
  - `docs/plans/2026-03-current-summary.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Inventory
- Current tracked `wave_b_*`: 24
- Concrete files still referenced by docs: 11
  - `wave_b_b2_closure_readiness_20260208_041500.md`\n  - `wave_b_b2_evidence_consistency_20260208_041500.md`\n  - `wave_b_b2_handoff_bundle_20260208_041500.md`\n  - `wave_b_ci_gate_summary_20260208_022636.md`\n  - `wave_b_ci_gate_summary_20260208_025426.md`\n  - `wave_b_ci_gate_summary_20260208_034029.md`\n  - `wave_b_cross_platform_summary_20260208_034029.md`\n  - `wave_b_cross_platform_summary_20260208_041500.md`\n  - `wave_b_macos_gate_probe_20260208.json`\n  - `wave_b_macos_gate_summary_20260208_0350.md`\n  - `wave_b_macos_gate_summary_20260208_041500.md`\n

## RED -> GREEN Plan
1. 新增 `tests/scripts/test_repo_hygiene_historical_wave_b_bucket_contract.sh`
2. 运行新合同，确认 RED
3. 创建 `docs/archive/reports/wave-b-history/`
4. 迁移 11 份仍被文档引用的 Wave B 历史证据到 archive 并更新 docs 引用
5. 删除其余 13 份未被引用的 replayable `wave_b_*`
6. 更新月度 manifest、summary 与 working memory
7. 接入 repo-hygiene batch / coverage contract 并运行 focused 验证
