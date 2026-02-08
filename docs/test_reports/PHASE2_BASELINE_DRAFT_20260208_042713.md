# Phase 2 性能基线草案报告（自动生成）

- 生成时间：2026-02-08 04:27:13 +0800
- 系统：Linux 6.12.63+deb13-amd64 #1 SMP PREEMPT_DYNAMIC Debian 6.12.63-1 (2025-12-30) x86_64 GNU/Linux
- FPC：3.3.1
- OpenSSL：OpenSSL 3.5.4 30 Sep 2025 (Library: OpenSSL 3.5.4 30 Sep 2025)

## 执行命令

```bash
bash /home/dtamade/projects/fafafa.ssl/tests/benchmarks/run_all_benchmarks.sh --iterations 50 --tls-iterations 10 --output /home/dtamade/projects/fafafa.ssl/tests/benchmarks/results --skip-tls
```

## 输出文件

- 汇总报告：`/home/dtamade/projects/fafafa.ssl/tests/benchmarks/results/benchmark_summary_20260208_042710.txt`
- 指标模板：`docs/test_reports/PHASE2_PERFORMANCE_METRICS_TEMPLATE.md`

## 汇总尾部（最近 80 行）

```text

Test                         Mean (ms)     P95 (ms)     P99 (ms)        Ops/s
---------------------------------------------------------------------------
aes_gcm_dec_16kb                 0.000        0.000        0.000          0.0
aes_gcm_dec_1kb                  0.000        0.000        0.000          0.0
aes_gcm_dec_64b                  0.000        0.000        0.000          0.0
aes_gcm_enc_16kb                 0.020        0.000        1.000      50000.0
aes_gcm_enc_1kb                  0.020        0.000        1.000      50000.0
aes_gcm_enc_64b                  0.000        0.000        0.000          0.0
generate_key_128bit              0.000        0.000        0.000          0.0
generate_key_256bit              0.000        0.000        0.000          0.0
secure_random_16kb               0.020        0.000        1.000      50000.0
secure_random_1kb                0.000        0.000        0.000          0.0
secure_random_64b                0.000        0.000        0.000          0.0
sha256_16kb                      0.060        1.000        1.000      16666.7
sha256_1kb                       0.000        0.000        0.000          0.0
sha256_64b                       0.000        0.000        0.000          0.0
sha512_16kb                      0.040        0.000        1.000      25000.0
sha512_1kb                       0.000        0.000        0.000          0.0
sha512_64b                       0.020        0.000        1.000      50000.0


Saving baseline to crypto_baseline.json...
Baseline saved

================================================================
Benchmark completed successfully
================================================================

=== benchmark_random_pool_20260208_042710.log ===


=================================
  Random Pool - 1 KB (Standard)
=================================

Random Pool Enabled (1KB x 10000)       :    40.00 ms  (    244.14 MB/s)
  Cache hit rate: 100.00%
  Refill count: 1250
Direct Generation (1KB x 10000)         :    99.00 ms  (     98.64 MB/s)


==================================================
  Random Pool - 4 KB (Boundary - MaxRequestSize)
==================================================

Random Pool Enabled (4KB x 1000)        :    16.00 ms  (    244.14 MB/s)
  Cache hit rate: 100.00%
  Refill count: 500
Direct Generation (4KB x 1000)          :    18.00 ms  (    217.01 MB/s)


==================================================
  Random Pool - 8 KB (Large - Direct Generation)
==================================================

Random Pool Enabled (8KB x 1000)        :    39.00 ms  (    200.32 MB/s)
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
```

## 下一步（B6）

1. 按模板填写关键指标（吞吐、延迟、回归阈值）。
2. 固化本次结果为基线，并补充对比说明。
3. 将结果同步到 docs/plans/ 与 task_plan.md。
