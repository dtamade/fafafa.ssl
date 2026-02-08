# Wave C B100 P1 Comparison（2026-02-08）

- focus: Cert Verify Cache (P1)
- b99_log: test-reports/wave_c_b99_cert_cache_run_20260208_043015.log
- b100_log: test-reports/wave_c_b100_cert_cache_run_20260208_043156.log

## 对照结果

| 指标 | B99 | B100 | 变化 |
|------|-----|------|------|
| cache hit rate (%) | 99.9 | 99.9 | 0.0 |
| without cache time (ms) | 59.0 | 56.0 | -3.0 |
| with cache time (ms) | 10.0 | 7.0 | -3.0 |
| speedup factor (x) | 5.9 | 8.0 | 2.1 |

## 验证命令

fpc -Mobjfpc -Sh -O2 -Fu./src -Fu./src/openssl -Fu./tests/benchmarks -Fu./examples -Fi./src -FE./tests/benchmarks/bin tests/benchmarks/benchmark_cert_verify_cache.pas

./tests/benchmarks/bin/benchmark_cert_verify_cache

## 结论

- P1 候选在重复探针中保持显著收益（B100 speedup = 8.0x）。
- 建议 B101 进入“与业务链路联动的最小接入验证”（保持回归门禁不回退）。
