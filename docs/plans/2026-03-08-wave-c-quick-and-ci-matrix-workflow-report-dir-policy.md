# Wave C quick and ci-matrix workflow report-dir policy Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收口剩余 workflow 层显式写 `test-reports/` 的两组手动/草案工作流：Wave C quick sprint manual 与 `ci-matrix-draft`。

**Architecture:** 这波只处理 workflow 层显式路径，不回头改历史 `test-reports/` 存量。Wave C quick sprint workflow 复用既有共享目录 `tmp/wave_c_quick_sprint_reports`；`ci-matrix-draft` 采用单独的 workflow 级目录 `tmp/ci_matrix_draft_reports`，其中 Linux 通过 `FAFAFA_TEST_REPORTS_DIR` 透传给 `run_all_module_tests.sh`，macOS/Windows 则把示例编译/运行日志落到同一树下，避免 artifact 继续指向旧目录。

**Tech Stack:** GitHub Actions YAML、Bash repo-hygiene contracts、Markdown working memory

---

## Scope
- Workflow:
  - `.github/workflows/wave-c-quick-sprint-manual.yml.disabled`
  - `.github/workflows/ci-matrix-draft.yml`
- Contracts:
  - `tests/scripts/test_repo_hygiene_workflow_wave_c_quick_and_ci_matrix_tmp_report_paths_contract.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch.sh`
  - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Working memory:
  - `docs/plans/2026-03-current-summary.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Architecture
- Wave C quick sprint workflow 活动目录：`tmp/wave_c_quick_sprint_reports`
- CI matrix draft workflow 活动目录：`tmp/ci_matrix_draft_reports`
- 原则：
  - workflow 层不再显式写 `test-reports/`
  - 手动/draft workflow 也遵守与脚本层一致的 `tmp/` 输出策略
  - `ci-matrix-draft` 的 Linux 分支应复用 `run_all_module_tests.sh` 现有 `FAFAFA_TEST_REPORTS_DIR` 透传能力，而不是新增脚本侧行为

## RED -> GREEN Plan
1. 新增 `tests/scripts/test_repo_hygiene_workflow_wave_c_quick_and_ci_matrix_tmp_report_paths_contract.sh`
2. 运行新合同，确认 RED
3. 修补 `.github/workflows/wave-c-quick-sprint-manual.yml.disabled`
4. 修补 `.github/workflows/ci-matrix-draft.yml`
5. 接入 repo-hygiene batch / coverage contract
6. 更新月度汇总与 working memory
7. 运行 focused contracts + batch + diff check，确认 GREEN

## Planned Commands
Run:

```bash
bash tests/scripts/test_repo_hygiene_workflow_wave_c_quick_and_ci_matrix_tmp_report_paths_contract.sh
```

Then:

```bash
bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh
bash tests/scripts/test_repo_hygiene_contract_batch.sh
git diff --check -- .github/workflows/wave-c-quick-sprint-manual.yml.disabled .github/workflows/ci-matrix-draft.yml tests/scripts/test_repo_hygiene_workflow_wave_c_quick_and_ci_matrix_tmp_report_paths_contract.sh tests/scripts/test_repo_hygiene_contract_batch.sh tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh docs/plans/2026-03-08-wave-c-quick-and-ci-matrix-workflow-report-dir-policy.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md
```

## Expected Outputs
- Wave C quick sprint workflow 的 output/upload paths 对齐 `tmp/wave_c_quick_sprint_reports`
- `ci-matrix-draft` 的 Linux/macOS/Windows 上传目录对齐 `tmp/ci_matrix_draft_reports`
- Repo-hygiene batch 对这两份 workflow 的 `test-reports/` 回流具备静态防回退保护
