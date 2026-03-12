# Historical `examples_compile*` + `tls13_signer_*` bucket cleanup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 清理历史 `examples_compile*` 与 `tls13_signer_*` 结果桶：将仍被活动文档引用的 examples 编译证据迁到归档区，删除未引用的 replayable 文件，并把无外部引用的 TLS13 signer local-smoke 产物全部从 `test-reports/` 跟踪面移除。

**Architecture:** 这一波合并处理两个小桶。`examples_compile*` 当前 10 份 tracked 文件中，9 份仍被 docs/plans/archive 文档具体点名，迁到 `docs/archive/reports/examples-compile-history/` 并更新引用；1 份未被引用的检查文本直接删除。`tls13_signer_*` 当前 8 份 tracked local-smoke 产物没有 `test-reports/` 外部具体引用，因此不归档，直接从 tracked 历史面删除。继续沿用月度 migration manifest，追加 Batch 4 记录移动与删除决策。

**Tech Stack:** Git file moves/deletions、Markdown manifest、Bash repo-hygiene contract

---

## Scope
- Historical files:
  - `test-reports/examples_compile*`
  - `test-reports/tls13_signer_*`
- Archive outputs:
  - `docs/archive/reports/examples-compile-history/`
  - `docs/archive/reports/2026-03-test-reports-migration-manifest.md`
- Contracts:
  - `tests/scripts/test_repo_hygiene_historical_examples_and_tls13_buckets_contract.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Working memory:
  - `docs/plans/2026-03-current-summary.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Inventory
- Current tracked `examples_compile*`: 10
- Concrete `examples_compile*` files still referenced by docs: 9
  - `examples_compile_after_b79_partial.json`
  - `examples_compile_ci_gate.json`
  - `examples_compile_gate_b83.json`
  - `examples_compile_gate_b84.json`
  - `examples_compile_gate_b85.json`
  - `examples_compile_gate_b86.json`
  - `examples_compile_gate_b87.json`
  - `examples_compile_gate_b88.json`
  - `examples_compile_latest.json`
- Unreferenced replayable `examples_compile*`: 1
  - `examples_compile_check_20260209_000601.txt`
- Current tracked `tls13_signer_*`: 8
- Concrete `tls13_signer_*` files still referenced outside `test-reports/`: 0

## RED -> GREEN Plan
1. 新增 `tests/scripts/test_repo_hygiene_historical_examples_and_tls13_buckets_contract.sh`
2. 运行新合同，确认 RED
3. 创建 `docs/archive/reports/examples-compile-history/`
4. 迁移 9 份仍被引用的 `examples_compile*` 历史证据到 archive 并更新所有具体引用
5. 删除 1 份未引用的 replayable `examples_compile*`
6. 删除 8 份无外部具体引用的 `tls13_signer_*` local-smoke 产物
7. 更新月度 manifest、summary 与 working memory
8. 接入 repo-hygiene batch / coverage contract 并运行 focused 验证
