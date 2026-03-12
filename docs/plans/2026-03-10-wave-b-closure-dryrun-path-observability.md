# 2026-03-10 Wave B closure dry-run path observability

## Goal
- 补齐 `check_wave_b_b2_closure_readiness.sh --dry-run` 对三平台 summary 路径的可观测面。
- 让 closure dry-run 不只给状态，还显式给出输入 summary 路径。

## Scope
- `scripts/check_wave_b_b2_closure_readiness.sh`
- `tests/scripts/test_wave_b_b2_closure_readiness_dryrun_paths_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 确认 closure dry-run path 缺口
- [x] 新增 focused shell contract
- [x] 最小补齐 dry-run 输出
- [x] 跑 focused + 既有 closure dry-run 回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n scripts/check_wave_b_b2_closure_readiness.sh tests/scripts/test_wave_b_b2_closure_readiness_dryrun_paths_contract.sh tests/scripts/test_wave_b_b2_closure_readiness_dryrun_skipped_semantics_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_b2_closure_readiness_dryrun_paths_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_b2_closure_readiness_dryrun_skipped_semantics_contract.sh` => PASS

## Result
- closure dry-run 现在会显式输出：
  - `linux_summary`
  - `macos_summary`
  - `windows_summary`
- 这样 dry-run 观察面在输入路径层也更完整，不再只有状态和值得用户自行反推路径。

## Next Queue
- 继续扫 Wave B 其它 producer/consumer 的 dry-run metadata 缺口。
- 或在 repo-noise 更低后回到 backend context/default-validation 架构复审。
