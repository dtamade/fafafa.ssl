# Performance Guides Benchmark Truth

## Goal

把 `docs/guides/PERFORMANCE_GUIDE.md` 和 `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
里的固定 benchmark/phase 结论、固定阈值、固定成功率快照收回到当前可执行 truth，
同时把性能示例里的旧 direct-core session/diagnostics 用法切回当前 owner-path 设计。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结两份性能文档的 benchmark truth 边界
- 只修改：
  - `docs/guides/PERFORMANCE_GUIDE.md`
  - `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
- 不改生产实现
- 不重开 WinSSL runtime proof 或旧的 Phase/完成度叙事

## Files

- Add: `docs/plans/2026-05-19-performance-guides-benchmark-truth.md`
- Add: `tests/scripts/test_performance_guides_benchmark_truth_contract.sh`
- Modify: `docs/guides/PERFORMANCE_GUIDE.md`
- Modify: `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

这两份性能文档当前同时存在两类 drift：

1. 把某次历史 benchmark/phase 跑数直接写成当前正文 truth：
   - 固定 `ops/s`
   - 固定 `ms` / `P99`
   - 固定 `提升 N 倍`
   - 固定 `目标值`
   - 固定 `完成 Phase X`
2. 性能示例还在教：
   - `ISSLConnection.GetSession`
   - `ISSLConnection.SetSession`
   - `ISSLConnection.IsSessionReused`
   - `ISSLConnection.GetPerformanceMetrics`
   但这些 core mirror 在当前仓库里已经被降级为 compatibility-only，
   active guidance 应优先走 `ISSLSessionResumption` / `ISSLDiagnostics`。

当前更可靠的真相源已经存在：

- `scripts/run_phase2_performance_baseline.sh`
- `tests/benchmarks/run_all_benchmarks.sh`
- `tests/benchmarks/baselines/*.json`
- `docs/test_reports/PHASE2_PERFORMANCE_METRICS_TEMPLATE.md`

## Verification

```bash
bash -n tests/scripts/test_performance_guides_benchmark_truth_contract.sh
bash tests/scripts/test_performance_guides_benchmark_truth_contract.sh
bash tests/scripts/test_active_docs_no_ci_pipeline_contract.sh
npx prettier --write docs/guides/PERFORMANCE_GUIDE.md docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md
git diff --check
```

## Expected Outcome

- `PERFORMANCE_GUIDE.md` 保留：
  - benchmark 入口
  - baseline/source truth
  - 成功标准
  - 结果解读边界
- `PERFORMANCE_OPTIMIZATION_GUIDE.md` 保留：
  - TLS 调优建议
  - session/diagnostics owner-path 示例
  - loopback vs network 的解读方法
- 但不再把固定历史跑数、phase 完成结论或 direct-core mirror 示例写成当前正文 truth
