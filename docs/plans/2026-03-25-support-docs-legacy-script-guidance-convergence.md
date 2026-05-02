# Support docs legacy script guidance convergence（2026-03-25）

## Goal

- 收口支持性文档里仍把历史 `build_linux.sh` / `run_tests_linux.sh` 当作默认验证动作的 drift。
- 统一这些 supporting docs 的 active guidance 到当前 canonical chain：
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`

## Architecture

- 这是一个 docs-only family。
- 只修正 supporting docs 中的当前入口和措辞，不重写整篇历史内容，也不改脚本实现。
- 对于明显带时间戳/历史评估语义的文档，保留其历史快照身份，只补充“当前该怎么验证”。

## Files

- `docs/plans/2026-03-25-support-docs-legacy-script-guidance-convergence.md`
- `docs/FCL_DEPENDENCIES.md`
- `docs/testing/TEST_COVERAGE_ASSESSMENT.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 复扫 supporting docs 中旧脚本命中，确认本批只覆盖：
   - `docs/FCL_DEPENDENCIES.md`
   - `docs/testing/TEST_COVERAGE_ASSESSMENT.md`
2. 更新 `docs/FCL_DEPENDENCIES.md`：
   - 把“使用我们的构建脚本”改成当前默认验证链路
   - 移除 `run_tests_linux.sh` active guidance
   - 明确历史脚本不再是默认入口
3. 更新 `docs/testing/TEST_COVERAGE_ASSESSMENT.md`：
   - 明确该文档是历史 assessment snapshot
   - 把 “Automated Test Suite” 的默认入口改成当前 canonical chain
4. 更新 `task_plan.md` / `findings.md` / `progress.md`，记录范围、结论与验证证据。

## Commands

```bash
rg -n "build_linux\\.sh|run_tests_linux\\.sh" docs/FCL_DEPENDENCIES.md docs/testing/TEST_COVERAGE_ASSESSMENT.md
rg -n "python3 scripts/compile_all_modules\\.py|bash scripts/run_minimal_ci_gate\\.sh --fast-local|bash scripts/run_phase2_performance_baseline\\.sh --dry-run --fast-local" docs/FCL_DEPENDENCIES.md docs/testing/TEST_COVERAGE_ASSESSMENT.md
git diff --check -- docs/plans/2026-03-25-support-docs-legacy-script-guidance-convergence.md docs/FCL_DEPENDENCIES.md docs/testing/TEST_COVERAGE_ASSESSMENT.md task_plan.md findings.md progress.md
```

## Expected Outputs

- supporting docs 不再把 `build_linux.sh` / `run_tests_linux.sh` 当作默认验证动作。
- `docs/FCL_DEPENDENCIES.md` 与 `docs/testing/TEST_COVERAGE_ASSESSMENT.md` 都指向当前 canonical chain。
- 历史 assessment 语义被保留，但不会再把读者带回旧脚本入口。
