# Phase 2 首轮基线对比结论（V1）

**结论级别**：Draft（流程验证完成，趋势结论需后续提高迭代次数复核）

---

## 1. 对比样本

| 样本 | 命令 | 汇总 |
|---|---|---|
| A（skip-tls） | `bash scripts/run_phase2_performance_baseline.sh --iterations 50 --tls-iterations 20 --skip-tls` | `tests/benchmarks/results/benchmark_summary_20260207_030221.txt` |
| B（with-tls） | `bash scripts/run_phase2_performance_baseline.sh --iterations 20 --tls-iterations 5 --with-tls` | `tests/benchmarks/results/benchmark_summary_20260207_030413.txt` |

> 注意：A/B 迭代参数不同，本轮对比仅用于“方向性判断”，不用于最终性能承诺。

---

## 2. 可比较指标（Random Pool）

| 指标 | A（skip-tls） | B（with-tls） | 变化 |
|---|---:|---:|---:|
| 1KB Pool 吞吐（MB/s） | 244.14 | 256.99 | +5.26% |
| 1KB Direct 吞吐（MB/s） | 117.66 | 120.56 | +2.46% |
| 4KB Pool 吞吐（MB/s） | 244.14 | 260.42 | +6.67% |
| 8KB Pool 吞吐（MB/s） | 252.02 | 244.14 | -3.13% |

解读：
- 小中块（1KB/4KB）池化优势继续存在；
- 8KB 属于 bypass 区间，波动在可接受范围内。

---

## 3. with-tls 样本读数（流程已打通）

来源：`tests/benchmarks/results/benchmark_tls_handshake_20260207_030413.log`

| 指标 | Mean(ms) | P95(ms) | Ops/s |
|---|---:|---:|---:|
| session_resumption | 2351.4 | 2440.0 | 0.4 |
| tls12_13_handshake | 2381.8 | 2402.0 | 0.4 |
| tls12_handshake | 2368.6 | 2413.0 | 0.4 |
| tls13_handshake | 2374.0 | 2400.0 | 0.4 |

---

## 4. 当前结论

1. Phase 2 基准链路已具备“可执行 + 可落盘 + 可追踪”能力。
2. Random Pool 指标方向稳定，未见异常退化。
3. TLS 样本已覆盖握手/会话项，但需在更高迭代与更稳定网络条件下复测。

---

## 5. 下一步建议

1. 统一迭代参数后重跑 A/B（建议 `iterations=200`, `tls-iterations=20`）。
2. 将本结论回填到 `docs/test_reports/PHASE2_PERFORMANCE_METRICS_TEMPLATE.md` 的“结论与行动”区。
3. 把 B10 的 OpenSSL 1.1.1/3.x 矩阵命令草案纳入 CI 门禁扩展。
