# Phase 2 基线执行汇总（2026-02-07）

**目的**：提供 B6 阶段基准采集的稳定入口（避免只依赖时间戳文件名）。

---

## 1. 执行批次

| 批次 | 命令 | 结果 | 汇总文件 | 草案报告 |
|---|---|---|---|---|
| B6-A（skip-tls） | `bash scripts/run_phase2_performance_baseline.sh --iterations 50 --tls-iterations 20 --skip-tls` | 2/2 通过 | `tests/benchmarks/results/benchmark_summary_20260207_030221.txt` | `docs/test_reports/PHASE2_BASELINE_DRAFT_20260207_030222.md` |
| B6-B（with-tls） | `bash scripts/run_phase2_performance_baseline.sh --iterations 20 --tls-iterations 5 --with-tls` | 3/3 通过 | `tests/benchmarks/results/benchmark_summary_20260207_030413.txt` | `docs/test_reports/PHASE2_BASELINE_DRAFT_20260207_030639.md` |

---

## 2. with-tls 样本关键读数

来源：`tests/benchmarks/results/benchmark_tls_handshake_20260207_030413.log`

| 指标 | Mean(ms) | P95(ms) | Ops/s |
|---|---:|---:|---:|
| session_resumption | 2351.4 | 2440.0 | 0.4 |
| tls12_13_handshake | 2381.8 | 2402.0 | 0.4 |
| tls12_handshake | 2368.6 | 2413.0 | 0.4 |
| tls13_handshake | 2374.0 | 2400.0 | 0.4 |

说明：本轮以“流程打通 + 指标落盘”为主，迭代次数较低（20/5），用于首轮基线快照。

---

## 3. 结论（当前）

1. Phase 2 基准链路已可重复执行（skip-tls 与 with-tls 两种模式均可跑通）。
2. 生成报告路径已稳定（汇总日志 + 草案报告双轨输出）。
3. 下一步进入 B9：将关键指标回填到模板并输出首轮对比结论。

---

## 4. 相关文档

- `docs/test_reports/PHASE2_PERFORMANCE_METRICS_TEMPLATE.md`
- `docs/plans/PHASE3_MINIMAL_CI_GATE_DRAFT.md`
- `scripts/run_phase2_performance_baseline.sh`
- `scripts/run_minimal_ci_gate.sh`
