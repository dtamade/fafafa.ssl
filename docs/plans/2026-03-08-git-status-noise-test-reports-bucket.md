# Git Status Noise Test-Reports Bucket (2026-03-08)

## Goal
- 把 `test-reports/*` 从 `git status` 摘要的泛化 `other` 桶里拆出来。
- 让下一波仓库卫生治理能直接量化测试报告产物漂移，而不是靠人工翻样例。

## Scope
- `scripts/summarize_git_status_noise_draft.sh`
- `tests/scripts/test_git_status_noise_summary_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
- 在现有摘要脚本中新增稳定分类 `test_reports_drift`。
- 保持脚本只读、非阻断，不改变任何脚本的默认输出路径。
- 用现有 contract fixture 扩展一个 `test-reports/sample.md` 变更，先锁定 RED，再做最小 GREEN。

## RED
- 更新 `tests/scripts/test_git_status_noise_summary_contract.sh`，要求报告出现 `| test_reports_drift | 1 |`。
- 运行 `bash tests/scripts/test_git_status_noise_summary_contract.sh`，预期失败。

## GREEN
- 更新 `scripts/summarize_git_status_noise_draft.sh`，将 `test-reports/*` 归类到 `test_reports_drift`。

## Regression
- `bash tests/scripts/test_git_status_noise_summary_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- `bash scripts/summarize_git_status_noise_draft.sh --output tmp/git_status_noise_summary_current.md`

## Result
- Updated `scripts/summarize_git_status_noise_draft.sh` to classify `test-reports/*` as `test_reports_drift`.
- Extended `tests/scripts/test_git_status_noise_summary_contract.sh` so the fixture now proves `test-reports` no longer collapses into `other`.
- Kept the change read-only and batch-compatible; no runtime or CI defaults changed.

## Verification
- `bash tests/scripts/test_git_status_noise_summary_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- `bash scripts/summarize_git_status_noise_draft.sh --output tmp/git_status_noise_summary_current.md`

## Review Snapshot
- live report: `tmp/git_status_noise_summary_current.md`
- `test_reports_drift`: `902`
- `other`: `1`
- `git ls-files -- test-reports`: `294`
- `docs/plans/*.md`: `354`
