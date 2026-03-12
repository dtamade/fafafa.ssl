# Workflow report-dir policy wave B and TLS13 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收口 Wave B / TLS13 相关 workflow 层仍显式写入 `test-reports/` 的活动输出路径，统一到 `tmp/wave_b_reports` 与 `tmp/tls13_signer_gate_reports`。

**Architecture:** 这波只处理 workflow 层的显式路径覆盖，不改历史 `test-reports/` 存量，也不回退脚本层已经完成的默认路径治理。做法是先加静态 repo-hygiene 合同锁定 workflow 字符串，再最小修改 `.github/workflows` 中 Linux/macOS/upload/summary 的显式路径，最后把新合同接入 repo-hygiene batch 与 working memory。

**Tech Stack:** GitHub Actions YAML、Bash repo-hygiene contracts、Markdown working memory

---

## Scope
- Workflow:
  - `.github/workflows/tls13-signer-gate.yml`
  - `.github/workflows/wave-b-b2-manual.yml.disabled`
- Contracts:
  - `tests/scripts/test_repo_hygiene_workflow_wave_b_tls13_tmp_report_paths_contract.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Working memory:
  - `docs/plans/2026-03-current-summary.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Architecture
- TLS13 signer workflow 活动目录：`tmp/tls13_signer_gate_reports`
- Wave B / B2 workflow 活动目录：`tmp/wave_b_reports`
- 原则：
  - workflow 层不再显式写 `test-reports/`
  - upload-artifact / step summary / summary-stage staging 必须与脚本默认目录一致
  - 历史兼容 `test-reports/` 清理留给单独历史面波次

## RED -> GREEN Plan
1. 新增 `tests/scripts/test_repo_hygiene_workflow_wave_b_tls13_tmp_report_paths_contract.sh`
2. 运行新合同，确认 RED
3. 修补 `.github/workflows/tls13-signer-gate.yml`
4. 修补 `.github/workflows/wave-b-b2-manual.yml.disabled`
5. 接入 repo-hygiene batch / coverage contract
6. 更新月度汇总与 working memory
7. 运行 focused contracts + batch + diff check，确认 GREEN

## Planned Commands
Run:

```bash
bash tests/scripts/test_repo_hygiene_workflow_wave_b_tls13_tmp_report_paths_contract.sh
```

Then:

```bash
bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh
bash tests/scripts/test_repo_hygiene_contract_batch.sh
git diff --check -- .github/workflows/tls13-signer-gate.yml .github/workflows/wave-b-b2-manual.yml.disabled tests/scripts/test_repo_hygiene_workflow_wave_b_tls13_tmp_report_paths_contract.sh tests/scripts/test_repo_hygiene_contract_batch.sh tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh docs/plans/2026-03-08-workflow-report-dir-policy-wave-b-and-tls13.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md
```

## Expected Outputs
- `tls13-signer-gate.yml` 的 bundle/upload/summary 全部对齐 `tmp/tls13_signer_gate_reports`
- `wave-b-b2-manual.yml.disabled` 的 gate/upload/summary/closure staging 全部对齐 `tmp/wave_b_reports`
- 新 workflow hygiene 合同能阻止 `test-reports/` 显式路径回流
- repo-hygiene batch 与 coverage contract 都包含本波新合同
