# Wave C baseline fast-local + active docs entrypoint drift（2026-03-15）

## Goal
- 让 `scripts/run_phase2_performance_baseline.sh` 在本地默认工作流里具备 clean-worktree 选项，并且 dry-run 不创建任何输出。
- 让 `scripts/run_minimal_ci_gate.sh --fast-local` 与 Phase 2 baseline dry-run 的输出口径保持一致。
- 清掉 active docs 中已经失效的 `ci_pipeline.sh` 用户入口，统一到当前真实门禁命令。

## Architecture / Approach
1. `run_phase2_performance_baseline.sh`
   - 增加 `--run-id`、`--fast-local`、`--doc-reports-dir`
   - `--dry-run` 仅打印解析后的路径与命令，不创建目录
   - 所有输出路径限制在仓库根目录下
2. `run_minimal_ci_gate.sh`
   - 当启用 `--fast-local` 时，向 Phase 2 baseline dry-run 透传 `--fast-local` 和同一 `run_id`
3. `cleanup_fast_local_outputs.sh`
   - 纳入 `tmp/phase2_bench_results_*` 清理候选
4. Active docs
   - README / Quickstart / Getting Started / Performance Guide 改用真实入口：
     - `python3 scripts/compile_all_modules.py`
     - `bash scripts/run_minimal_ci_gate.sh --fast-local`
     - `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`

## Files
- `scripts/run_phase2_performance_baseline.sh`
- `scripts/run_minimal_ci_gate.sh`
- `scripts/cleanup_fast_local_outputs.sh`
- `README.md`
- `docs/guides/GETTING_STARTED.md`
- `docs/guides/QUICKSTART.md`
- `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
- `tests/scripts/test_run_phase2_performance_baseline_fast_local_dry_run_contract.sh`
- `tests/scripts/test_run_minimal_ci_gate_phase2_fast_local_passthrough_contract.sh`
- `tests/scripts/test_active_docs_no_ci_pipeline_contract.sh`

## Step-by-step Commands
1. Script syntax:
   - `bash -n scripts/run_phase2_performance_baseline.sh scripts/run_minimal_ci_gate.sh scripts/cleanup_fast_local_outputs.sh`
2. Contract checks:
   - `bash tests/scripts/test_run_phase2_performance_baseline_fast_local_dry_run_contract.sh`
   - `bash tests/scripts/test_run_minimal_ci_gate_phase2_fast_local_passthrough_contract.sh`
   - `bash tests/scripts/test_cleanup_fast_local_outputs_safe_defaults_contract.sh`
   - `bash tests/scripts/test_active_docs_no_ci_pipeline_contract.sh`
3. Focused gate smoke:
   - `bash scripts/run_minimal_ci_gate.sh --fast-local --skip-compile --skip-modules`

## Expected Outputs
- `run_phase2_performance_baseline.sh --dry-run --fast-local` 输出的 benchmark/report 路径都位于 `./tmp/`
- `run_minimal_ci_gate.sh --fast-local` 中的 Phase 2 dry-run 与上面使用同一 `run_id`、同一 `tmp` 路径
- active docs 不再引用不存在的 `ci_pipeline.sh`
- cleanup helper 可以识别 Phase 2 baseline fast-local 产物目录
