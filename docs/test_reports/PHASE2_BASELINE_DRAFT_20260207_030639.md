# Phase 2 性能基线草案报告（自动生成）

- 生成时间：2026-02-07 03:06:39 +0800
- 系统：Linux 6.12.63+deb13-amd64 #1 SMP PREEMPT_DYNAMIC Debian 6.12.63-1 (2025-12-30) x86_64 GNU/Linux
- FPC：3.3.1
- OpenSSL：OpenSSL 3.5.4 30 Sep 2025 (Library: OpenSSL 3.5.4 30 Sep 2025)

## 执行命令

```bash
bash /home/dtamade/projects/fafafa.ssl/tests/benchmarks/run_all_benchmarks.sh --iterations 20 --tls-iterations 5 --output /home/dtamade/projects/fafafa.ssl/tests/benchmarks/results
```

## 输出文件

- 汇总报告：`/home/dtamade/projects/fafafa.ssl/tests/benchmarks/results/benchmark_summary_20260207_030413.txt`
- 指标模板：`docs/test_reports/PHASE2_PERFORMANCE_METRICS_TEMPLATE.md`

## 汇总尾部（最近 80 行）

```text
==================================================
  Random Pool - 8 KB (Large - Direct Generation)
==================================================

Random Pool Enabled (8KB x 1000)        :    32.00 ms  (    244.14 MB/s)
  Cache hit rate: 0.00% (expected: 0% - bypasses pool)
  Cache misses: 1000
Direct Generation (8KB x 1000)          :    34.00 ms  (    229.78 MB/s)

╔════════════════════════════════════════════════════════╗
║  Random Pool Performance Summary                      ║
╚════════════════════════════════════════════════════════╝

Phase B 优化目标：2-5x 性能提升

关键发现：
  - 小数据块 (256B-1KB): 缓存池显著提升性能
  - 边界场景 (4KB): 接近 MaxRequestSize，性能提升明显
  - 大数据块 (8KB): 自动绕过缓存池，性能相当

配置建议：
  - PoolSize: 8KB (默认)
  - RefillThreshold: 1KB (默认)
  - MaxRequestSize: 4KB (默认)

╔════════════════════════════════════════════════════════╗
║  Benchmark Completed Successfully                     ║
╚════════════════════════════════════════════════════════╝

=== benchmark_tls_handshake_20260207_030413.log ===
[2026-02-07 03:05:30.883] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:05:33.279] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:05:35.674] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:05:38.046] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:05:40.419] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:05:42.797] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:05:45.157] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:05:47.529] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:05:49.903] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:05:52.287] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:05:54.645] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:05:57.001] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:05:59.361] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:01.717] [INFO] [OpenSSL] SSL Context created (Type: 0)
  tls12_handshake: 2368.600 ms/op (stddev: 22.249, ops/s: 0.4)
[2026-02-07 03:06:04.130] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:06.503] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:08.851] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:11.226] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:13.609] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:15.965] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:18.338] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:20.730] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:23.101] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:25.487] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:27.845] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:30.246] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:32.611] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:34.988] [INFO] [OpenSSL] SSL Context created (Type: 0)
[2026-02-07 03:06:37.362] [INFO] [OpenSSL] SSL Context created (Type: 0)
  tls13_handshake: 2374.000 ms/op (stddev: 15.697, ops/s: 0.4)

================================================================

=== Benchmark Results ===

Test                         Mean (ms)     P95 (ms)     P99 (ms)        Ops/s
---------------------------------------------------------------------------
session_resumption            2351.400     2440.000     2440.000          0.4
tls12_13_handshake            2381.800     2402.000     2402.000          0.4
tls12_handshake               2368.600     2413.000     2413.000          0.4
tls13_handshake               2374.000     2400.000     2400.000          0.4


Saving baseline to tls_handshake_baseline.json...
Baseline saved

================================================================
Benchmark completed successfully
================================================================
```

## 下一步（B6）

1. 按模板填写关键指标（吞吐、延迟、回归阈值）。
2. 固化本次结果为基线，并补充对比说明。
3. 将结果同步到 docs/plans/ 与 task_plan.md。
