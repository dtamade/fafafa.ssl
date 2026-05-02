# Wave C cert verify cache baseline integration（2026-03-15）

## Goal
- 把 `benchmark_cert_verify_cache` 从单独的旁路脚本/报告入口，收敛到统一的 benchmark runner 与 Phase 2 baseline 流程里。
- 让这条性能链路在 `--fast-local` 下继续保持 clean-worktree：日志、summary、benchmark bin 都写到 `./tmp/`。

## Architecture / Approach
1. `tests/benchmarks/run_all_benchmarks.sh`
   - 增加 `--bin-dir`
   - 将 benchmark 运行时工作目录切到 `--output`，避免生成物散落到调用目录
   - 将 `benchmark_cert_verify_cache` 纳入默认 benchmark 集合
2. `tests/benchmarks/benchmark_cert_verify_cache.pas`
   - 支持从命令行读取迭代次数
   - fixture 路径不再依赖当前工作目录，优先读取 `FAFAFA_PROJECT_ROOT`，否则向上搜索项目根
   - 修复低迭代 / `0ms` 时间窗口下的浮点除零
3. `scripts/run_phase2_performance_baseline.sh`
   - 增加 `--bin-dir`
   - `--fast-local` 时将 benchmark bin 输出到 `tmp/phase2_bench_bin_<run_id>`
4. `scripts/cleanup_fast_local_outputs.sh`
   - 纳入 `phase2_bench_bin_*`

## Files
- `tests/benchmarks/run_all_benchmarks.sh`
- `tests/benchmarks/benchmark_cert_verify_cache.pas`
- `scripts/run_phase2_performance_baseline.sh`
- `scripts/cleanup_fast_local_outputs.sh`
- `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
- `tests/scripts/test_run_all_benchmarks_cert_verify_cache_contract.sh`
- `tests/scripts/test_run_phase2_performance_baseline_fast_local_dry_run_contract.sh`
- `tests/scripts/test_run_minimal_ci_gate_phase2_fast_local_passthrough_contract.sh`
- `tests/scripts/test_cleanup_fast_local_outputs_safe_defaults_contract.sh`

## Step-by-step Commands
1. Syntax / compile sanity:
   - `bash -n tests/benchmarks/run_all_benchmarks.sh scripts/run_phase2_performance_baseline.sh scripts/run_minimal_ci_gate.sh scripts/cleanup_fast_local_outputs.sh`
   - `fpc -Fu./src -Fu./src/openssl -Fu./tests/benchmarks -Fu./examples -Fi./src tests/benchmarks/benchmark_cert_verify_cache.pas -otmp/benchmark_cert_verify_cache_smoke`
2. Contracts:
   - `bash tests/scripts/test_run_all_benchmarks_cert_verify_cache_contract.sh`
   - `bash tests/scripts/test_run_phase2_performance_baseline_fast_local_dry_run_contract.sh`
   - `bash tests/scripts/test_run_minimal_ci_gate_phase2_fast_local_passthrough_contract.sh`
   - `bash tests/scripts/test_cleanup_fast_local_outputs_safe_defaults_contract.sh`
3. Focused execution:
   - `bash scripts/run_phase2_performance_baseline.sh --fast-local --iterations 1 --tls-iterations 1 --skip-tls`
   - `bash scripts/run_minimal_ci_gate.sh --fast-local --skip-compile --skip-modules`

## Expected Outputs
- `run_all_benchmarks.sh` summary 中出现 `benchmark_cert_verify_cache`，且日志写到指定 `--output`
- `benchmark_cert_verify_cache` 在低迭代下仍稳定退出 0
- `run_phase2_performance_baseline.sh --fast-local` 将 benchmark results / bin / docs report 都限定到 `./tmp`
