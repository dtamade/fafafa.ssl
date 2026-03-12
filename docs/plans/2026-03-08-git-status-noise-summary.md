# Git Status Noise Summary (2026-03-08)

## Goal
- 增加一个轻量脚本，把 `git status --short` 的工作区噪音按类别汇总，帮助审阅大工作区时快速识别重点。
- 重点覆盖：根目录 `bin/*` 删除面、workflow 漂移、docs 漂移、source edits，以及其他杂项。

## Scope
- `scripts/summarize_git_status_noise_draft.sh`
- `tests/scripts/test_git_status_noise_summary_contract.sh`
- `tests/scripts/test_repo_hygiene_contract_batch.sh`
- `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
- 新增一个只读脚本，解析 `git status --short --untracked-files=all` 输出并生成 Markdown 摘要报告。
- 使用稳定分类：
  - `generated_artifacts_root_bin`
  - `workflow_drift`
  - `docs_drift`
  - `source_edits`
  - `scripts_drift`
  - `tests_drift`
  - `examples_drift`
  - `worktree_meta`
  - `other`
- 保持脚本非阻断；用于可观测性和 reviewer 导航，不改变运行时行为。

## RED
1. Add:
   - `tests/scripts/test_git_status_noise_summary_contract.sh`
2. Run:
   - `bash tests/scripts/test_git_status_noise_summary_contract.sh`
   - Expected: FAIL，因为摘要脚本尚不存在。

## GREEN
1. Add:
   - `scripts/summarize_git_status_noise_draft.sh`
2. Add the new contract to:
   - `tests/scripts/test_repo_hygiene_contract_batch.sh`
   - `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`

## Regression
- `bash tests/scripts/test_git_status_noise_summary_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`

## Result
- Added `scripts/summarize_git_status_noise_draft.sh` as a read-only Markdown reporter for `git status --short --untracked-files=all`.
- Added `tests/scripts/test_git_status_noise_summary_contract.sh` and wired it into both repo-hygiene batch entrypoints.
- Kept the change non-blocking and reviewer-focused: the new report improves navigation without changing runtime behavior or CI defaults.

## Verification
- `bash tests/scripts/test_git_status_noise_summary_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- `bash scripts/summarize_git_status_noise_draft.sh --output tmp/git_status_noise_summary_current.md`

## Review Snapshot
- Live report: `tmp/git_status_noise_summary_current.md`
- `git status` entries: `1806`
- `generated_artifacts_root_bin`: `134`
- `docs_drift`: `236`
- `tests_drift`: `342`
- `other`: `903`
- `git ls-files -- bin`: `0`
- `git ls-files -- test-reports`: `294`
- `docs/plans/*.md`: `353`
